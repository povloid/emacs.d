## как отрубить network-manager

Иногда это необходимо что-бы контролировать и настраивать сеть по старому

``
systemctl disable NetworkManager
``

---

## Как отрубить переход в спящий режим.

Это надо если машина должна быть постоянно доступна в сети.

[статья](https://synay.net/en/support/kb/disabling-automatic-workstation-sleep-debian-12)


When using the Debian 12 operating system in workstation mode with a desktop (Gnome or KDE) installed,
by default the operating system goes into sleep mode when there is no user activity.

Since our goal is to make the workstation always accessible over the network,
it is necessary to disable the operating system from going into sleep mode in the absence of user activity.

1. Normal (old way)

``
systemctl mask sleep.target suspend.target hibernate.target hybrid-sleep.target
``


2. Recommended (new method)
Create a file:

``
mkdir -p /etc/systemd/sleep.conf.d/
touch /etc/systemd/sleep.conf.d/nosuspend.conf
``

The following content:

``
[Sleep]
AllowSuspend=no
AllowHibernation=no
AllowSuspendThenHibernate=no
AllowHybridSleep=no
``

Done, now the workstation will work around the clock in server mode.

---

## Установка поддержки тумб для различных файлов

``
sudo apt install ffmpegthumbnailer tumbler tumbler-plugins-extra tumbler-common
``
