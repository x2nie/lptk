-- For MySQL

CREATE TABLE feladat
(
  sorszam   INTEGER NOT NULL auto_increment,
  temaszam  VARCHAR(8),
  felelos   VARCHAR(20),
  prioritas SMALLINT,
  hatarido  DATETIME,
  datum     DATETIME,
  jelzes    VARCHAR(6),
  megnev    VARCHAR(60),
  megj      TEXT,
  megoldva  CHAR(1),
  rejtett   CHAR(1),
  PRIMARY KEY (sorszam)
);

CREATE TABLE szemely
(
  kod       VARCHAR(8) NOT NULL,
  nev       VARCHAR(30),
  PRIMARY KEY (kod)
);