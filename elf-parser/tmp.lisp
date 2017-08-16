(show-memory-sec '( "LOAD" "text" "rodata" "datas" "bss" "noinit"))
(show-debug-module-symbols
 '( "ext/lib/actions/libsystem"
   "samples/storyapp/src/storyplayer"
   "samples/storyapp/src/ota"
   "samples/storyapp/src/system"
   "samples/storyapp/src/networkpair"
   "samples/storyapp/src/playtts"
   "samples/storyapp/src/sair"
   "samples/storyapp/src/messager"
   "samples/storyapp"
   "usb"
   "ext/lib/actions/json"
   "ext/lib/actions/libnethelper"
   "ext/lib/actions/downloadservice"
   "ext/lib/actions/mqttservice"
   "ext/lib/actions/musicservice"
   "ext/lib/actions/systemshell"
   "ext/lib/actions/recordservice"
   "ext/lib/actions/btdrv"
   "ext/lib/actions/wechat"
   "ext/lib/actions"
   "ext/lib/crypto"
   "ext/fs"
   "libc/"
   "subsys/net/lib"
   "subsys/net"
   "subsys/shell"
   "subsys/disk"
   "subsys/fs"
   "subsys/usb"
   "subsys/bluetooth"
   "kernel"
   "arch/"
   "misc"
   "wifi/host"
   "drivers/adc"
   "drivers/audio"
   "drivers/bluetooth"
   "drivers/console"
   "drivers/flash"
   "drivers/gpio"
   "drivers/input"
   "drivers/mmc"
   "drivers/nvram"
   "drivers/timer"
   "drivers/wifi"
   "drivers/usb"
   "drivers"
   "fs"
   "src/"
   "./../"
   "sinvoice")
 :prefix "/home/local/ACTIONS/zhangdf/GL5118_WIFI"
 :dump-file nil)

(show-debug-module-symbols
 '(    "ext/lib/crypto"
   "ext/fs"
   "libc/"
   "subsys/net"
   "subsys/bluetooth"
   "kernel"
   "arch/"
   "misc"
   "drivers/gpio"
   "drivers/timer"
   "src/"   )
 :prefix "/home/local/ACTIONS/zhangdf/GL5118_WIFI"
 :dump-file nil)


(defparameter *file* #p "/Users/zhangdongfeng/Downloads/airaha/AB1520S_SVN72747_Headset_OBJ/output/AB1520S/Release_Flash/BTStereoHeadset_AB1520S_FlashLinkRom.MAP")


#+or
(with-open-file (f #p "/Users/zhangdongfeng/Downloads/airaha/AB1520S_SVN72747_Headset_OBJ/output/AB1520S/Release_Flash/BTStereoHeadset_AB1520S_FlashLinkRom.MAP1"  :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (loop for line in *lines*
     do (write-line line  f)))
