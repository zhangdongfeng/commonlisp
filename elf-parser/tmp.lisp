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
 '("ext/lib/crypto"
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


(defparameter %merge-publics% "\\bMERGEPUBLICS CLASSES\\b")
(defparameter %seg-name% "\\b\\[A-Z_]+\\b")
(defparameter %data-addr% "\\bD?:?0X[0-9A-F]{2,8}-D?:?0X[0-9A-F]{2,8}\\b")

(defparameter  *merge-publics-spec*
  '(%merge-publics% (%seg-name% (%data-addr%))))

(defun remove-regex (str regex)
  (multiple-value-bind (s  e  r1 r2)
      (scan regex  str)
    (if e  (values (subseq str e) t)
        (values str nil))))

(defun read-remove-regex-from-string (regex string collector)
  (let ((s1 (remove-regex string "^\\s+" )))
    (multiple-value-bind (m r)
        (scan-to-strings (concatenate 'string "^" regex) s1)
      (if m
          (read-remove-regex-from-string
           regex
           (remove-regex (remove-regex s1 regex) ",")
           #'(lambda (result str)
               (funcall collector (cons m result) str)))
          (funcall collector nil string)))))

(defun  parse-regex-spec (spec  str)
  (let ((regex (car spec))
        (s1 (remove-regex str "^\\s+\\(\\s+" )))
    (if (listp regex)
        (multiple-value-bind (r s2)
            (parse-regex-spec regex s1)
          (let ((s3 (remove-regex s2 "^\\s+\\)\\s+" )))
            (multiple-value-bind (r1 s4)
                (parse-regex-spec (cdr spec) s3)
              (values (cons r r1) s4))))
        (read-remove-regex-from-string  regex s1
                                        #'(lambda (res str)
                                            (multiple-value-bind (rr ss1)
                                                (parse-regex-spec (cdr spec)  s1)
                                              (values (cons rr res) ss1)))))))


"
MERGEPUBLICS CLASSES
(
 BIT (D:0X22-D:0X2F),
     DATA (D:0X0C-0X1F, 0X30-D:0X7F),
     EDATA (0X9607-0XEFFF),
     EDATA_HIEDATA (0XF000-0XFFDF),
     EDATA_PATCH_PARA (0XFFE0-0XFFFF),
     ECODE_STARTUP (0X800040-0X800FFF),
     HCONST_MP_PARAM (0X801000-0X801FFF),
     HCONST_MP_PARAM_F (0X801000-0X801FFF),
     HCONST_DESCRIP (0X802000-0X8020FF),
     HCONST_CFGHEAD (0X802100-0X8022FF),
     CODE (0X802300-0X80FFFF),
     ECODE (0X802300-0X84FFFF),
     HCONST (0X802300-0X84FFFF),
     ECODE_FLASH (0X802300-0X84FFFF),
     HCONST_FLASH (0X802300-0X84FFFF),
     HCONST_DSP_ROM (0X850000-0X87FFFF),
     HCONST_CONFIG (0X880000-0X88FFFF),
     HCONST_VO_DAT (0X890000-0X8F5FFF),
     HCONST_DSP_FUNCPARA (0X8F6000-0X8F6FFF),
     HCONST_DSP_HPFPARA (0X8F7000-0X8F7FFF),
     HCONST_DSP_PEQPARA (0X8F7000-0X8F7FFF),
     HCONST_APP_CALLNAME (0X8F8000-0X8F8FFF),
     HCONST_MMI_V_NVRAM2 (0X8F9000-0X8F9FFF),
     HCONST_MMI_C_V_NVRAM2 (0X8F9600-0X8F9FFF),
     HCONST_MMI_V_NVRAM (0X8FA000-0X8FAFFF),
     HCONST_MMI_C_V_NVRAM (0X8FA600-0X8FAFFF))
SEGMENTS
(?C_INITEDATA,?C_INITEDATA_END)
OVERLAY (
         OS_SCHEDULER !
                      (KERNEL??,
                       LM,
                       GAP,
                       L2CAP,
                       SDAP,
                       RFCOMM,
                       AVCTP,
                       AVDTP,
                       A2DP,
                       AVRCP,
                       MMI,
                       MMI_LE,
                       ATT,
                       SM,
                       HFP,
                       DRIVER,
                       SPP),
                      MMI_STATE_PASSTHRUMSG !
                      (MMI_CONNECTDISCOVERABLEHANDLER,
                       MMI_CONNECTABLEHANDLER,
                       MMI_CONNECTEDHANDLER,
                       MMI_HFPIMGHANDLER,
                       MMI_HFPOGGHANDLER,
                       MMI_HFPCAHANDLER,
                       MMI_HFPCAOGGHANDLER,
                       MMI_HFPCAIMGHANDLER,
                       MMI_HFPCAMULTYHANDLER,
                       MMI_TESTMODEHANDLER,
                       MMI_OFFHANDLER,
                       MMI_FAKEONHANDLER,
                       MMI_FAKEOFFHANDLER,
                       MMI_DETACHHANDLER,
                       MMI_LINEINHANDLER,
                       MMI_FMHANDLER),
                      MMI_LOADHANDLER !
                      (MMI_CONNECTDISCOVERABLEENTRY,
                       MMI_CONNECTABLEENTRY,
                       MMI_CONNECTEDENTRY,
                       MMI_HFPIMGENTRY,
                       MMI_HFPOGGENTRY,
                       MMI_HFPCAENTRY,
                       MMI_HFPCAIMGENTRY,
                       MMI_TESTMODEENTRY,
                       MMI_OFFENTRY,
                       MMI_FAKEONENTRY,
                       MMI_FAKEOFFENTRY,
                       MMI_DETACHENTRY,
                       MMI_LINEINENTRY,
                       MMI_FMENTRY,
                       MMI_VOICEPROMPTLANGSELECTENTRY,
                       MMI_TWS_PAIRINGENTRY),
                      MMI_UNLOADHANDLER !
                      (MMI_CONNECTEDEXIT,
                       MMI_CONNECTDISCOVERABLEEXIT,
                       MMI_HFPCAEXIT,
                       MMI_HFPIMGEXIT,
                       MMI_TESTMODEEXIT,
                       MMI_DETACHEXIT,
                       MMI_VOICEPROMPTLANGSELECTEXIT,
                       MMI_LINEINEXIT,
                       MMI_FMEXIT,
                       MMI_TWS_PAIRINGEXIT),
                      MMI_TESTMODE_EXECUTE !
                      (MMI_RXTEST,
                       MMI_TXCONTINUEPACKET,
                       MMI_TXBURSTPACKET,
                       MMI_TXSINGLETONE),
                      LM_POWERCONTROL !
                      (LM_SENDINCREASEPOWERREQHANDLER,
                       LM_SENDDECREASEPOWERREQHANDLER),
                      DRIVER_PROCMMICMD !
                      (DRIVER_MMIREADYCMDHANDLER,
                       DRIVER_UPDATEGENERALPARACMDHANDLER,
                       DRIVER_SETNORMALVOLUMECMDHANDLER,
                       DRIVER_SETA2DPVOLUMECMDHANDLER,
                       DRIVER_SETLINEINVOLUMECMDHANDLER,
                       DRIVER_BUZZERCMDHANDLER,
                       DRIVER_RINGTONEVPCMDHANDLER,
                       DRIVER_STOPSPECIFICRINGCMDHANDLER,
                       DRIVER_STOPSPECIFICVPCMDHANDLER,
                       DRIVER_STOPRINGCMDHANDLER,
                       DRIVER_STOPVPCMDHANDLER,
                       DRIVER_FAKEMEDIAVPRINGCMDHANDLER,
                       DRIVER_SCOCMDHANDLER,
                       DRIVER_A2DPCMDHANDLER,
                       DRIVER_LINEINCMDHANDLER,
                       DRIVER_STOPLINEINCMDHANDLER,
                       DRIVER_STOPDSPCMDHANDLER,
                       DRIVER_STOPDSPPOWEROFFCMDHANDLER,
                       DRIVER_POWERONCLOSEADDACMDHANDLER,
                       DRIVER_BATTERYSTATUSCMDHANDLER,
                       DRIVER_VOICECOMMANDHANDLER,
                       DRIVER_STOPVOICECOMMANDHANDLER,
                       DRIVER_SETAUDIOSELHANDLER),
                      MAILBOX_CMDHANDLER !
                      (ONESTEP_SUBSTATEHANDLER??,
                       PROMPTSTART_SUBSTATEHANDLER??,
                       PROMPTSTOP_SUBSTATEHANDLER??),
                      MAILBOX_EVTHANDLER !
                      (CMD_REPLY_EVTHANDLER,
                       VP_END_EVTHANDLER,
                       RT_END_EVTHANDLER,
                       VC_END_EVTHANDLER),
                      CMD_REPLY_EVTHANDLER !
                      (ONESTEP_SUBSTATEHANDLER??,
                       PROMPTSTART_SUBSTATEHANDLER??,
                       PROMPTSTOP_SUBSTATEHANDLER??))
OVERLAY (
         * !
           (SDAP_GETSERVICEATTRIBUTE?,
            SYSMODE_CTLR_INIT?,
            SYSMODE_DEVICE_INIT,
            _LM_LINKQUEUEINIT?)) SPEEDOVL NOSORTSIZE NOOVERLAY"
