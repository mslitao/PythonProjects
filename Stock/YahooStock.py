from yahoo_finance import Share

yahoo = Share('YHOO')

print yahoo.get_open()
print yahoo.get_historical('2016-06-01', '2016-06-20')
