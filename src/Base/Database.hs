module Base.Database where

import Base.Types

connection = connectSqlite3 "duchessMain.db"