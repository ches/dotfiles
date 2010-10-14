" http://github.com/godlygeek/tabular
if exists(':Tabularize')
  " Like Cucumber tables
  AddTabularPattern! bars /|/l1

  AddTabularPattern! hashrockets /=>/l1
endif
