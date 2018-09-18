import pandas as pd
import datetime, dateutil.parser

df = pd.read_csv("01.consAllx.csv", sep=";")
#df = pd.read_csv("00.consAll.csv", sep=";")
#df = pd.read_csv("/srv/shiny-server/appShiny/consNow.csv", sep=";")

df.columns = ['ID','date', 'energy']

formatDate = lambda da: dateutil.parser.parse(da)#.strftime('%Y-%m-%d %H:%M:%S.%f')
df['date'] = df['date'].apply(formatDate)
df['energy'] = df['energy'].astype(int)

df.sort_values('date', inplace=True)
df.set_index('date', drop=True).plot()
df.drop_duplicates(subset='energy', inplace=True)
df['energy'] = df['energy'] / 1000 
#df.set_index('date', drop=True).plot()
# Now we delete the values that are smaller than the previous
df = df[[True] + [True if x<y else False for x,y in zip(df['energy'][:-1],
                                                            df['energy'][1:])]]
df.date = [x.replace(microsecond=0) for x in df.date]
df.set_index('date', drop=True, inplace=True)
df = df.diff()


df['tim_dif'] = pd.to_datetime(pd.Series(df.index),utc=True).diff().values
df['tim_dif'] = df.tim_dif.dt.seconds
df['energy'] = df['energy'] / df['tim_dif']
df2 = df[['energy']]
df2 = df2.reindex(pd.date_range(df2.index[0], df2.index[-1], freq='1s'))
df2 = df2.fillna(method='bfill')

df2 = df2.resample('H').sum()
#df2 = df2.resample('d').sum()
df2['date'] = df2.index

#df2.to_csv("/srv/shiny-server/appShiny/aggCons.csv",sep=";", index=False)
#df2.to_csv("/srv/shiny-server/appShiny/aggConsNow.csv",sep=";", index=False)
df2.to_csv("02.aggConsPerHour.csv",sep=";", index=False)

