#!/data/local/tmp/cwm/sh

echo remount rw /system
mount -o remount rw /system

echo copy recovery.tar to system.
cat /data/local/tmp/cwm/recovery.tar > /system/bin/recovery.tar
chmod 644 /system/bin/recovery.tar

echo copy battery_charging to system.
cat /data/local/tmp/cwm/battery_charging > /system/bin/battery_charging
chmod 755 /system/bin/battery_charging

echo copy battery_charging_help to system.
cat /data/local/tmp/cwm/battery_charging_help > /system/bin/battery_charging_help
chmod 755 /system/bin/battery_charging_help

echo copy sh to system.
cat /data/local/tmp/cwm/sh > /system/xbin/sh
chmod 755 /system/xbin/sh

echo remount ro /system
mount -o remount ro /system
