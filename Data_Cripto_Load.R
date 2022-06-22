
#########################################################################################################
# Remove all

rm(list=ls())

#########################################################################################################
# Load libraries

library(BatchGetSymbols)
library(lubridate)
library(tidyverse)

#########################################################################################################
# Define dates and data frequency

first.date <- ymd(20180101,tz="UTC") # Start in 01-01-2018
last.date <- ymd(20211231,tz="UTC") # Finish 31-12-2021
freq.data <- 'daily'

#########################################################################################################
# Define tickers of 266 cryptocurrencies (not all will be available right now)

tickers <- c('BTC-USD','ETH-USD','BNB-USD','XRP-USD','USDT-USD','ADA-USD','DOT1-USD','LTC,USD','LINK-USD','BCH-USD',
             'XLM-USD','USDC-USD','TRX-USD','DOGE-USD','MIOTA-USD','SOL1-USD','EOS-USD','LUNA1-USD','XRM-USD','BSV-USD',
             'XTZ-USD','NEO-USD','ATOM1-USD','AVAX-USD','XEM-USD','ALGO-USD','KSM-USD','CTC1-USD','EGLD-USD','DASH-USD',
             'HBAR-USD','DCR-USD','ETC-USD','BTG-USD','ZEC-USD','TFUEL-USD','BAT-USD','STX1-USD','CCXX-USD','CEL-USD',
             'QTUM-USD','STEEM-USD','KMD-USD','HNT1-USD','OMG-USD','WAVES-USD','ICX-USD','DFI-USD','ZRX-USD','BNT-USD',
             'ONE2-USD','SC-USD','DGB-USD','XWC-USD','AR-USD','CELO-USD','MTL-USD','VGX-USD','LSK-USD','XVG-USD',
             'ZEN-USD','NANO-USD','CKB-USD','XDC-USD','LRC-USD','KNC-USD','STORJ-USD','SNT-USD','EWT-USD','CVC-USD',
             'ETN-USD','GLM-USD','MAID-USD','REP-USD','VLX-USD','ANT-USD','KIN-USD','FUN-USD','ARK-USD','BTS-USD',
             'COTI-USD','SYS-USD','GNO-USD','META-USD','BTM-USD','MARO-USD','RLC-USD','PPT-USD','HNC-USD','HNS-USD',
             'HIVE-USD','AION-USD','RDD-USD','IRIS-USD','DNT-USD','MWC-USD','ATRI-USD','WTC-USD','MLN-USD','MCO-USD',
             'NYE-USD','PHA-USD','CRU-USD','VRA-USD','MONA-USD','DIVI-USD','REV-USD','SAPP-USD','ADX-USD','TT-USD',
             'GNR-USD','FIRO-USD','GAS-USD','ABBC-USD','NULS-USD','WOZX-USD','PIVX-USD','PCX-USD','NXS-USD','ZNN-USD',
             'XNC-USD','DCN-USD','VSYS-USD','BEAM-USD','KDA-USD','MASS-USD','FIO-USD','SKY-USD','AE-USD','ERG-USD',
             'AXEL-USD','NAS-USD','HC-USD','ARRR-USD','SERO-USD','EMC2-USD','HC-USD','GXC-USD','MHC-USD','OBSR-USD',
             'NAV-USD','NEBL-USD','FNS-USD','BIP-USD','NXT-USD','XLT-USD','DMCH-USD','RBTC-USD','KRT-USD','GAME-USD',
             'SBD-USD','VTC-USD','DGD-USD','ETP-USD','SALT-USD','QRL-USD','VERI-USD','PAY-USD','VIA-USD','CUT-USD',
             'ETP-USD','NMC-USD','ACH-USD','DYN-USD','SFT-USD','PPC-USD','BURST-USD','GBYTE-USD','SMART-USD','DYN-USD',
             'PZM-USD','BHD-USD','WGR-USD','XDN-USD','ACT-USD','UBQ-USD','SNM-USD','YOYOW-USD','FCT-USD','NVT-USD',
             'SNGLS-USD','GLEEC-USD','CET-USD','AMB-USD','TERA-USD','VAL1-USD','FLO-USD','ZANO-USD','BTC2-USD','AYA-USD',
             'AEON-USD','NYZO-USD','WINGS-USD','PART-USD','FTC-USD','CTC-USD','DMD-USD','BPS-USD','HTDF-USD','NLG-USD',
             'XMY-USD','OTO-USD','DTEP-USD','IDNA-USD','ILC-USD','XST-USD','SCP-USD','DIME-USD','XRC-USD',
             'SCP-USD','PMEER-USD','BLK-USD','GRC-USD','FAIR-USD','CRW-USD','BPC-USD','CURE-USD','SUB-USD','IOC-USD',
             'USNBT-USD','MBC-USD','ZVC-USD','MGO-USD','XAS-USD','GCC1-USD','SONO1-USD','XAS-USD','OWC-USD','MBC-USD',
             'ERK-USD','CPS-USD','GHOST-USD','EDG-USD','COMP1-USD','DDK-USD','NPC-USD','OURO-USD','HSS-USD',
             'UNO-USD','ALIAS-USD','MINT-USD','ECC-USD','FLASH-USD','NLC2-USD','CLAM-USD','XUC-USD','COLX-USD','DUN-USD',
             'FLASH-USD','RBY-USD','MRX-USD','SHIFT-USD','MTC2-USD','CCA-USD','MIDAS-USD','JDC-USD','SLS-USD',
             'XNS1-USD','DCY-USD','BTX-USD','XCP-USD','LRG-USD','BCR-USD','XLQ-USD','YEP-USD','VBK-USD')

#########################################################################################################
# Load data

l_out <- BatchGetSymbols(tickers=tickers,
                         first.date=first.date,
                         last.date=last.date, 
                         freq.data=freq.data,
                         cache.folder=file.path(tempdir(),'BGS_Cache'))

data <-data.frame(l_out[["df.tickers"]][["ref.date"]],
                  l_out[["df.tickers"]][["price.close"]],
                  l_out[["df.tickers"]][["ticker"]])
colnames(data) <- c("Date","Price.close","Ticker")
sort(table(data$Ticker))
data$Ticker <- as.factor(data$Ticker)
length(levels.default(data$Ticker))

# There are 153 cryptocurrencies from 01-01-2018 to 31-12-2021 (some may have NAs anyway)

#########################################################################################################
# Define an object with columns the prices of the cryptocurrencies and write a csv file

dataComplete <- pivot_wider(data,names_from=Ticker,values_from=Price.close)
setwd("G:\\Mi unidad\\MSDS\\TFM")
write_csv2(dataComplete,"Price_close_Crypto.csv")
