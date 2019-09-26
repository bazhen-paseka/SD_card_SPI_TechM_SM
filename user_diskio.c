/* USER CODE BEGIN Header */
/**
 ******************************************************************************
  * @file    user_diskio.c
  * @brief   This file includes a diskio driver skeleton to be completed by the user.
  ******************************************************************************
  * @attention
  *
  * <h2><center>&copy; Copyright (c) 2019 STMicroelectronics.
  * All rights reserved.</center></h2>
  *
  * This software component is licensed by ST under Ultimate Liberty license
  * SLA0044, the "License"; You may not use this file except in compliance with
  * the License. You may obtain a copy of the License at:
  *                             www.st.com/SLA0044
  *
  ******************************************************************************
  */
 /* USER CODE END Header */

#ifdef USE_OBSOLETE_USER_CODE_SECTION_0
/* 
 * Warning: the user section 0 is no more in use (starting from CubeMx version 4.16.0)
 * To be suppressed in the future. 
 * Kept to ensure backward compatibility with previous CubeMx versions when 
 * migrating projects. 
 * User code previously added there should be copied in the new user sections before 
 * the section contents can be deleted.
 */
/* USER CODE BEGIN 0 */
/* USER CODE END 0 */
#endif

/* USER CODE BEGIN DECL */

/* Includes ------------------------------------------------------------------*/
#include <string.h>
#include "ff_gen_drv.h"
#include "main.h"
/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* CS pin settings */
#ifndef FATFS_CS_PIN
#define FATFS_CS_PORT						CS_GPIO_Port
#define FATFS_CS_PIN						CS_Pin
#endif
/* CS pin */
#define FATFS_CS_LOW()						FATFS_CS_PORT -> BSRR = FATFS_CS_PIN << 16
#define FATFS_CS_HIGH()						FATFS_CS_PORT -> BSRR = FATFS_CS_PIN
/* MMC card type flags (MMC_GET_TYPE) */
#define CT_MMC		0x01		/* MMC ver 3 */
#define CT_SD1		0x02		/* SD ver 1 */
#define CT_SD2		0x04		/* SD ver 2 */
#define CT_SDC		(CT_SD1|CT_SD2)	/* SD */
#define CT_BLOCK	0x08		/* Block addressing */
/* SDCARD block size */
#define SD_BLOCK_SIZE     512
/* MMC/SD command */
#define CMD0	(0)			/* GO_IDLE_STATE */
#define CMD1	(1)			/* SEND_OP_COND (MMC) */
#define	ACMD41	(0x80+41)	/* SEND_OP_COND (SDC) */
#define CMD8	(8)			/* SEND_IF_COND */
#define CMD9	(9)			/* SEND_CSD */
#define CMD10	(10)		/* SEND_CID */
#define CMD12	(12)		/* STOP_TRANSMISSION */
#define ACMD13	(0x80+13)	/* SD_STATUS (SDC) */
#define CMD16	(16)		/* SET_BLOCKLEN */
#define CMD17	(17)		/* READ_SINGLE_BLOCK */
#define CMD18	(18)		/* READ_MULTIPLE_BLOCK */
#define CMD23	(23)		/* SET_BLOCK_COUNT (MMC) */
#define	ACMD23	(0x80+23)	/* SET_WR_BLK_ERASE_COUNT (SDC) */
#define CMD24	(24)		/* WRITE_BLOCK */
#define CMD25	(25)		/* WRITE_MULTIPLE_BLOCK */
#define CMD32	(32)		/* ERASE_ER_BLK_START */
#define CMD33	(33)		/* ERASE_ER_BLK_END */
#define CMD38	(38)		/* ERASE */
#define CMD55	(55)		/* APP_CMD */
#define CMD58	(58)		/* READ_OCR */
/* Private variables ---------------------------------------------------------*/
/* SD Card SPI handle */
SPI_HandleTypeDef *hspi_sd;
/* Disk status */
static volatile DSTATUS Stat = STA_NOINIT;
static BYTE CardType = 0; /* Card type flags */
static BYTE dummyByte = 0xFF;

/* SDCARD detect function */
static uint8_t SDCARD_IsDetected(void) {
	/* Card is detected */
	return 1;
}

/* SDCARD write protect function */
static uint8_t SDCARD_IsWriteEnabled(void) {
	/* Card is not write protected */
	return 1;
}

/* Receive multiple byte */
static void rcvr_spi_multi(BYTE *buff, /* Pointer to data buffer */
UINT btr /* Number of bytes to receive (even number) */
) {
	/* Read multiple bytes, send 0xFF as dummy */
	for (UINT i = 0; i < btr; i++) {
		*(buff + i) = 0xFF;
	}

	HAL_SPI_TransmitReceive(hspi_sd, buff, buff, btr, 100);
}

#if _USE_WRITE
/* Send multiple byte */
static void xmit_spi_multi(const BYTE *buff, /* Pointer to the data */
UINT btx /* Number of bytes to send (even number) */
) {
	/* Write multiple bytes */
	HAL_SPI_Transmit(hspi_sd, (uint8_t *)buff, btx, 100);
}
#endif

/*-----------------------------------------------------------------------*/
/* Wait for card ready                                                   */
/*-----------------------------------------------------------------------*/
static int wait_ready( /* 1:Ready, 0:Timeout */
UINT wt /* Timeout [ms] */
) {
	BYTE d;
	uint32_t tick = HAL_GetTick();
	do {
		HAL_SPI_TransmitReceive(hspi_sd, &dummyByte, &d, 1, 100);
	} while (d != 0xFF && HAL_GetTick() - tick < wt); /* Wait for card goes ready or timeout */

	return (d == 0xFF) ? 1 : 0;
}

/*-----------------------------------------------------------------------*/
/* Deselect card and release SPI                                         */
/*-----------------------------------------------------------------------*/
static void sd_deselect(void) {
	FATFS_CS_HIGH(); /* CS = H */
	HAL_SPI_Transmit(hspi_sd, &dummyByte, 1, 100); /* Dummy clock (force DO hi-z for multiple slave SPI) */
}

/*-----------------------------------------------------------------------*/
/* Select card and wait for ready                                        */
/*-----------------------------------------------------------------------*/
static int sd_select(void) /* 1:OK, 0:Timeout */
{
	FATFS_CS_LOW();
	HAL_SPI_Transmit(hspi_sd, &dummyByte, 1, 100); /* Dummy clock (force DO enabled) */

	if (wait_ready(500)) {
		return 1; /* OK */
	}
	sd_deselect();
	return 0; /* Timeout */
}

/*-----------------------------------------------------------------------*/
/* Receive a data packet from the MMC                                    */
/*-----------------------------------------------------------------------*/
static int rcvr_datablock( /* 1:OK, 0:Error */
BYTE *buff, /* Data buffer */
UINT btr /* Data block length (byte) */
) {
	BYTE token;

	uint32_t tick = HAL_GetTick();
	do {						// Wait for DataStart token in timeout of 200ms
		HAL_SPI_TransmitReceive(hspi_sd, &dummyByte, &token, 1, 100);
	} while ((token == 0xFF) && HAL_GetTick() - tick < 200);
	if (token != 0xFE) {
		return 0;		// Function fails if invalid DataStart token or timeout
	}

	rcvr_spi_multi(buff, btr);		// Store trailing data to the buffer
	HAL_SPI_Transmit(hspi_sd, &dummyByte, 1, 100);
	HAL_SPI_Transmit(hspi_sd, &dummyByte, 1, 100);			// Discard CRC
	return 1;						// Function succeeded
}

/*-----------------------------------------------------------------------*/
/* Send a data packet to the MMC                                         */
/*-----------------------------------------------------------------------*/
#if _USE_WRITE
static int xmit_datablock( /* 1:OK, 0:Failed */
const BYTE *buff, /* Ponter to SD_BLOCK_SIZE byte data to be sent */
BYTE token /* Token */
) {
	BYTE resp;

	if (!wait_ready(500)) {
		return 0; /* Wait for card ready */
	}

	HAL_SPI_Transmit(hspi_sd, &token, 1, 100); /* Send token */
	if (token != 0xFD) { /* Send data if token is other than StopTran */
		xmit_spi_multi(buff, SD_BLOCK_SIZE); /* Data */
		HAL_SPI_Transmit(hspi_sd, &dummyByte, 1, 100);
		HAL_SPI_Transmit(hspi_sd, &dummyByte, 1, 100); /* Dummy CRC */
		resp = 0xFF;
		HAL_SPI_TransmitReceive(hspi_sd, &resp, &resp, 1, 100); /* Receive data resp */
		if ((resp & 0x1F) != 0x05) /* Function fails if the data packet was not accepted */
			return 0;
	}
	return 1;
}
#endif

/*-----------------------------------------------------------------------*/
/* Send a command packet to the MMC                                      */
/*-----------------------------------------------------------------------*/
static BYTE send_cmd( /* Return value: R1 resp (bit7==1:Failed to send) */
BYTE cmd, /* Command index */
DWORD arg /* Argument */
) {
	BYTE n, resp;

	if (cmd & 0x80) { /* Send a CMD55 prior to ACMD<n> */
		cmd &= 0x7F;
		resp = send_cmd(CMD55, 0);
		if (resp > 1)
			return resp;
	}

	/* Select the card and wait for ready except to stop multiple block read */
	if (cmd != CMD12) {
		sd_deselect();
		if (!sd_select())
			return 0xFF;
	}

	/* Send command packet */
	BYTE bufTx[5];
	bufTx[0] = 0x40 | cmd;
	bufTx[1] = (BYTE) (arg >> 24);
	bufTx[2] = (BYTE) (arg >> 16);
	bufTx[3] = (BYTE) (arg >> 8);
	bufTx[4] = (BYTE) arg;
	HAL_SPI_Transmit(hspi_sd, bufTx, 5, 100);
	n = 0x01; /* Dummy CRC + Stop */
	if (cmd == CMD0)
		n = 0x95; /* Valid CRC for CMD0(0) */
	if (cmd == CMD8)
		n = 0x87; /* Valid CRC for CMD8(0x1AA) */
	HAL_SPI_Transmit(hspi_sd, &n, 1, 100);

	/* Receive command resp */
	if (cmd == CMD12) {
		HAL_SPI_Transmit(hspi_sd, &dummyByte, 1, 100); /* Discard following one byte when CMD12 */
	}

	n = 10; /* Wait for response (10 bytes max) */
	do {
		HAL_SPI_TransmitReceive(hspi_sd, &dummyByte, &resp, 1, 100);
	} while ((resp & 0x80) && --n);

	return resp; /* Return received response */
}
/* USER CODE END DECL */

/* Private function prototypes -----------------------------------------------*/
DSTATUS USER_initialize (BYTE pdrv);
DSTATUS USER_status (BYTE pdrv);
DRESULT USER_read (BYTE pdrv, BYTE *buff, DWORD sector, UINT count);
#if _USE_WRITE == 1
  DRESULT USER_write (BYTE pdrv, const BYTE *buff, DWORD sector, UINT count);  
#endif /* _USE_WRITE == 1 */
#if _USE_IOCTL == 1
  DRESULT USER_ioctl (BYTE pdrv, BYTE cmd, void *buff);
#endif /* _USE_IOCTL == 1 */

Diskio_drvTypeDef  USER_Driver =
{
  USER_initialize,
  USER_status,
  USER_read, 
#if  _USE_WRITE
  USER_write,
#endif  /* _USE_WRITE == 1 */  
#if  _USE_IOCTL == 1
  USER_ioctl,
#endif /* _USE_IOCTL == 1 */
};

/* Private functions ---------------------------------------------------------*/

/**
  * @brief  Initializes a Drive
  * @param  pdrv: Physical drive number (0..)
  * @retval DSTATUS: Operation status
  */
DSTATUS USER_initialize (
	BYTE pdrv           /* Physical drive nmuber to identify the drive */
)
{
  /* USER CODE BEGIN INIT */
    Stat = STA_NOINIT;
    BYTE n, cmd, ty, ocr[4];

	/* Set CS high */
	FATFS_CS_HIGH();

	/* Wait for stable */
	HAL_Delay(10);

	if (!SDCARD_IsDetected()) {
		return STA_NODISK;
	}

	for (n = 10; n; n--) {
		HAL_SPI_Transmit(hspi_sd, &dummyByte, 1, 100);
	}
	ty = 0;
	if (send_cmd(CMD0, 0) == 1) { /* Put the card SPI/Idle state */
		if (send_cmd(CMD8, 0x1AA) == 1) { /* SDv2? */
			for (n = 0; n < 4; n++) {
				HAL_SPI_TransmitReceive(hspi_sd, &dummyByte, ocr + n, 1, 100); /* Get 32 bit return value of R7 resp */
			}
			if (ocr[2] == 0x01 && ocr[3] == 0xAA) { /* Is the card supports vcc of 2.7-3.6V? */
				while (send_cmd(ACMD41, 1UL << 30))
					; /* Wait for end of initialization with ACMD41(HCS) */
				if (send_cmd(CMD58, 0) == 0) { /* Check CCS bit in the OCR */
					for (n = 0; n < 4; n++) {
						HAL_SPI_TransmitReceive(hspi_sd, &dummyByte, ocr + n, 1,
								100);
					}
					ty = (ocr[0] & 0x40) ? CT_SD2 | CT_BLOCK : CT_SD2; /* Card id SDv2 */
				}
			}
		} else { /* Not SDv2 card */
			if (send_cmd(ACMD41, 0) <= 1) { /* SDv1 or MMC? */
				ty = CT_SD1;
				cmd = ACMD41; /* SDv1 (ACMD41(0)) */
			} else {
				ty = CT_MMC;
				cmd = CMD1; /* MMCv3 (CMD1(0)) */
			}

			send_cmd(cmd, 0); /* Wait for end of initialization */

			if (send_cmd(CMD16, SD_BLOCK_SIZE) != 0) { /* Set block length: SD_BLOCK_SIZE */
				ty = 0;
			}
		}
	}
	CardType = ty; /* Card type */
	sd_deselect();

	if (ty) { /* OK */
		Stat &= ~STA_NOINIT; /* Clear STA_NOINIT flag */
	} else { /* Failed */
		Stat = STA_NOINIT;
	}

	if (!SDCARD_IsWriteEnabled()) {
		Stat |= STA_PROTECT;
	} else {
		Stat &= ~STA_PROTECT;
	}

	return Stat;
  /* USER CODE END INIT */
}
 
/**
  * @brief  Gets Disk Status 
  * @param  pdrv: Physical drive number (0..)
  * @retval DSTATUS: Operation status
  */
DSTATUS USER_status (
	BYTE pdrv       /* Physical drive number to identify the drive */
)
{
  /* USER CODE BEGIN STATUS */
	Stat = STA_NOINIT;
	/* Check card detect pin if enabled */
	if (!SDCARD_IsDetected()) {
		return STA_NODISK;
	} else {
		Stat &= ~STA_NOINIT;
	}

	/* Check if write is enabled */
	if (!SDCARD_IsWriteEnabled()) {
		Stat |= STA_PROTECT;
	} else {
		Stat &= ~STA_PROTECT;
	}

	return Stat; /* Return disk status */
  /* USER CODE END STATUS */
}

/**
  * @brief  Reads Sector(s) 
  * @param  pdrv: Physical drive number (0..)
  * @param  *buff: Data buffer to store read data
  * @param  sector: Sector address (LBA)
  * @param  count: Number of sectors to read (1..128)
  * @retval DRESULT: Operation result
  */
DRESULT USER_read (
	BYTE pdrv,      /* Physical drive nmuber to identify the drive */
	BYTE *buff,     /* Data buffer to store read data */
	DWORD sector,   /* Sector address in LBA */
	UINT count      /* Number of sectors to read */
)
{
  /* USER CODE BEGIN READ */
    if (!SDCARD_IsDetected() || (Stat & STA_NOINIT)) {
		return RES_NOTRDY;
	}

	if (!(CardType & CT_BLOCK)) {
		sector *= SD_BLOCK_SIZE; /* LBA ot BA conversion (byte addressing cards) */
	}

	if (count == 1) { /* Single sector read */
		BYTE ret = send_cmd(CMD17, sector);
		int read_ret = rcvr_datablock(buff, SD_BLOCK_SIZE);
		if ((ret == 0) && read_ret)
			count = 0;
	} else { /* Multiple sector read */
		if (send_cmd(CMD18, sector) == 0) { /* READ_MULTIPLE_BLOCK */
			do {
				if (!rcvr_datablock(buff, SD_BLOCK_SIZE)) {
					break;
				}
				buff += SD_BLOCK_SIZE;
			} while (--count);
			send_cmd(CMD12, 0); /* STOP_TRANSMISSION */
		}
	}
	sd_deselect();

	return count ? RES_ERROR : RES_OK; /* Return result */
  /* USER CODE END READ */
}

/**
  * @brief  Writes Sector(s)  
  * @param  pdrv: Physical drive number (0..)
  * @param  *buff: Data to be written
  * @param  sector: Sector address (LBA)
  * @param  count: Number of sectors to write (1..128)
  * @retval DRESULT: Operation result
  */
#if _USE_WRITE == 1
DRESULT USER_write (
	BYTE pdrv,          /* Physical drive nmuber to identify the drive */
	const BYTE *buff,   /* Data to be written */
	DWORD sector,       /* Sector address in LBA */
	UINT count          /* Number of sectors to write */
)
{ 
  /* USER CODE BEGIN WRITE */
  	if (!SDCARD_IsDetected()) {
		return RES_ERROR;
	}
	if (!SDCARD_IsWriteEnabled()) {
		return RES_WRPRT;
	}
	if (Stat & STA_NOINIT) {
		return RES_NOTRDY; /* Check drive status */
	}
	if (Stat & STA_PROTECT) {
		return RES_WRPRT; /* Check write protect */
	}

	if (!(CardType & CT_BLOCK)) {
		sector *= SD_BLOCK_SIZE; /* LBA ==> BA conversion (byte addressing cards) */
	}

	if (count == 1) { /* Single sector write */
		if ((send_cmd(CMD24, sector) == 0) /* WRITE_BLOCK */
		&& xmit_datablock(buff, 0xFE))
			count = 0;
	} else { /* Multiple sector write */
		if (CardType & CT_SDC)
			send_cmd(ACMD23, count); /* Predefine number of sectors */
		if (send_cmd(CMD25, sector) == 0) { /* WRITE_MULTIPLE_BLOCK */
			do {
				if (!xmit_datablock(buff, 0xFC)) {
					break;
				}
				buff += SD_BLOCK_SIZE;
			} while (--count);
			if (!xmit_datablock(0, 0xFD)) { /* STOP_TRAN token */
				count = 1;
			}
		}
	}
	sd_deselect();

	return count ? RES_ERROR : RES_OK; /* Return result */
  /* USER CODE END WRITE */
}
#endif /* _USE_WRITE == 1 */

/**
  * @brief  I/O control operation  
  * @param  pdrv: Physical drive number (0..)
  * @param  cmd: Control code
  * @param  *buff: Buffer to send/receive control data
  * @retval DRESULT: Operation result
  */
#if _USE_IOCTL == 1
DRESULT USER_ioctl (
	BYTE pdrv,      /* Physical drive nmuber (0..) */
	BYTE cmd,       /* Control code */
	void *buff      /* Buffer to send/receive control data */
)
{
  /* USER CODE BEGIN IOCTL */
	DRESULT res = RES_ERROR;
	BYTE n, csd[16];
	DWORD *dp, st, ed, csize;

	if (Stat & STA_NOINIT) {
		return RES_NOTRDY; /* Check if drive is ready */
	}
	if (!SDCARD_IsDetected()) {
		return RES_NOTRDY;
	}

	res = RES_ERROR;

	switch (cmd) {
	case CTRL_SYNC: /* Wait for end of internal write process of the drive */
		if (sd_select())
			res = RES_OK;
		break;

		/* Size in bytes for single sector */
	case GET_SECTOR_SIZE:
		*(WORD *) buff = SD_BLOCK_SIZE;
		res = RES_OK;
		break;

	case GET_SECTOR_COUNT: /* Get drive capacity in unit of sector (DWORD) */
		if ((send_cmd(CMD9, 0) == 0) && rcvr_datablock(csd, 16)) {
			if ((csd[0] >> 6) == 1) { /* SDC ver 2.00 */
				csize = csd[9] + ((WORD) csd[8] << 8)
						+ ((DWORD) (csd[7] & 63) << 16) + 1;
				*(DWORD*) buff = csize << 10;
			} else { /* SDC ver 1.XX or MMC ver 3 */
				n = (csd[5] & 15) + ((csd[10] & 128) >> 7) + ((csd[9] & 3) << 1)
						+ 2;
				csize = (csd[8] >> 6) + ((WORD) csd[7] << 2)
						+ ((WORD) (csd[6] & 3) << 10) + 1;
				*(DWORD*) buff = csize << (n - 9);
			}
			res = RES_OK;
		}
		break;

	case GET_BLOCK_SIZE: /* Get erase block size in unit of sector (DWORD) */
		if (CardType & CT_SD2) { /* SDC ver 2.00 */
			if (send_cmd(ACMD13, 0) == 0) { /* Read SD status */
				HAL_SPI_Transmit(hspi_sd, &dummyByte, 1, 100);
				if (rcvr_datablock(csd, 16)) { /* Read partial block */
					for (n = 64 - 16; n; n--)
						HAL_SPI_Transmit(hspi_sd, &dummyByte, 1, 100); /* Purge trailing data */
					*(DWORD*) buff = 16UL << (csd[10] >> 4);
					res = RES_OK;
				}
			}
		} else { /* SDC ver 1.XX or MMC */
			if ((send_cmd(CMD9, 0) == 0) && rcvr_datablock(csd, 16)) { /* Read CSD */
				if (CardType & CT_SD1) { /* SDC ver 1.XX */
					*(DWORD*) buff = (((csd[10] & 63) << 1)
							+ ((WORD) (csd[11] & 128) >> 7) + 1)
							<< ((csd[13] >> 6) - 1);
				} else { /* MMC */
					*(DWORD*) buff =
							((WORD) ((csd[10] & 124) >> 2) + 1)
									* (((csd[11] & 3) << 3)
											+ ((csd[11] & 224) >> 5) + 1);
				}
				res = RES_OK;
			}
		}
		break;

	case CTRL_TRIM: /* Erase a block of sectors (used when _USE_ERASE == 1) */
		if (!(CardType & CT_SDC))
			break; /* Check if the card is SDC */
		if (USER_ioctl(pdrv, MMC_GET_CSD, csd))
			break; /* Get CSD */
		if (!(csd[0] >> 6) && !(csd[10] & 0x40))
			break; /* Check if sector erase can be applied to the card */
		dp = buff;
		st = dp[0];
		ed = dp[1]; /* Load sector block */
		if (!(CardType & CT_BLOCK)) {
			st *= SD_BLOCK_SIZE;
			ed *= SD_BLOCK_SIZE;
		}
		if (send_cmd(CMD32, st) == 0 && send_cmd(CMD33, ed) == 0
				&& send_cmd(CMD38, 0) == 0 && wait_ready(30000)) /* Erase sector block */
			res = RES_OK; /* FatFs does not check result of this command */
		break;

	default:
		res = RES_PARERR;
	}

	sd_deselect();

	return res;
  /* USER CODE END IOCTL */
}
#endif /* _USE_IOCTL == 1 */

/************************ (C) COPYRIGHT STMicroelectronics *****END OF FILE****/
