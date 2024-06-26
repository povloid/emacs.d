#!/bin/bash

# Взято отсюда
# https://qastack.ru/apple/113954/difference-between-autopoweroff-and-standby-in-pmset

# Эта спящая / спящая вещь сводила меня с ума. И я согласен, что Apple
# должна приложить больше усилий для исправления такого поведения pmset.

# Я буду ясен. Мне нравится режим гибернации, и он отлично работал в старых
# моделях и ОС (достаточно было просто запустить sudo pmset hibernatemode 25).
# Теперь Apple что-то сломала, и это не работает со времен Yosemite

# Сейчас у меня есть Retina, и теперь более, чем когда-либо, режим гибернации
# имеет больше смысла, чем Sleep. Я не понимаю, почему люди с твердотельными
# накопителями так сильно любят Sleep over hibernate, я просто не понимаю,
# что они не понимают разницу времени пробуждения между режимом сна и сна в
# 1 секунду, но они экономят тонны батареи, кто-то объяснит мне, что они
# видят большое преимущество, потому что я его не понимаю. В любом случае,
# (если вы любите спящий режим так же сильно, как и я, продолжайте чтение),
# я хотел, чтобы спящий режим работал. Мне потребовались недели,
# чтобы заставить это работать, и я поделюсь тем, что я сделал со всеми вами.

# Вы должны сбросить SMC, а затем NVRAM / PRAM:

# Перезагрузите SMC ( http://support.apple.com/kb/ht3964 ):

# Выключить компьютер.
# Подключите адаптер питания MagSafe к источнику питания, подключив его к Mac, если он еще не подключен.
# На встроенной клавиатуре одновременно нажмите клавиши Shift-Control-Option и кнопку питания, расположенные слева.
# Отпустите все клавиши и кнопку питания одновременно.
# Нажмите кнопку питания, чтобы включить компьютер.
# Примечание . Индикатор на адаптере питания MagSafe может изменить состояние или временно отключиться при сбросе SMC.

# Сброс NVRAM / PRAM ( http://support.apple.com/kb/HT1379 ):

# Выключи свой Mac.
# Найдите следующие клавиши на клавиатуре: Command, Option, P и R. Вам нужно будет одновременно удерживать эти клавиши на шаге 4.
# Включить компьютер.
# Нажмите и удерживайте клавиши Command-Option-PR до появления серого экрана.
# Удерживайте клавиши, пока компьютер не перезагрузится, и вы не услышите звук запуска во второй раз.
# Отпустите ключи.

# Теперь выполните эти:

# Все:

sudo pmset -a sleep 0
sudo pmset -a standby 0
sudo pmset -a standbydelay 0
sudo pmset -a standbydelayhigh 0
sudo pmset -a standbydelaylow 0
sudo pmset -a hibernatemode 25
sudo pmset -a highstandbythreshold 20
sudo pmset -a acwake 0
sudo pmset -a lidwake 0
sudo pmset -a ttyskeepawake 0
sudo pmset -a darkwakes 0

# AC:

# sudo pmset -c sleep 0
# sudo pmset -c standby 0
# sudo pmset -c standbydelay 0
# sudo pmset -c hibernatemode 25

# Батарея:

sudo pmset -b sleep 0
sudo pmset -b standby 0
sudo pmset -b standbydelay 5
sudo pmset -b hibernatemode 25


# Always use the integrated graphics card while running on battery power
sudo pmset -b gpuswitch 0

# Always use the discrete graphics card while running on battery power
# sudo pmset -b gpuswitch 1

# Switch between discrete and integrated graphics cards automatically while running on battery power
# sudo pmset -b gpuswitch 2
