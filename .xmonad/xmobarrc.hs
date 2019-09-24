Config {
       font = "xft:Zekton:size=10:bold:antialias=true"
       , additionalFonts = [ "xft:FontAwesome:size=10"  -- https://fontawesome.com/cheatsheet
                           , "xft:IcoFont:normal"
                           ]
         
       , allDesktops = False
       , bgColor = "#282c34"
       , fgColor = "#bbc2cf"
       , position = TopW L 95
       , commands = [ Run Cpu [ "--template", "<fc=#a9a1e1><fn=1></fn></fc> <total>%"
                              , "--Low","3"
                              , "--High","50"
                              , "--low","#bbc2cf"
                              , "--normal","#bbc2cf"
                              , "--high","#fb4934"] 50

                    , Run Memory ["-t","<fc=#51afef><fn=1></fn></fc> <usedratio>%"
                                 ,"-H","80"
                                 ,"-L","10"
                                 ,"-l","#bbc2cf"
                                 ,"-n","#bbc2cf"
                                 ,"-h","#fb4934"] 50

                    , Run Date "<fc=#ECBE7B><fn=1></fn></fc> %a %b %_d %H:%M" "date" 300
                    , Run DynNetwork ["-t","<fc=#4db5bd><fn=1></fn></fc> <rx>, <fc=#c678dd><fn=1></fn></fc> <tx>"
                                     ,"-H","200"
                                     ,"-L","10"
                                     ,"-h","#bbc2cf"
                                     ,"-l","#bbc2cf"
                                     ,"-n","#bbc2cf"] 50

                    , Run CoreTemp ["-t", "<fc=#CDB464><fn=1></fn></fc> <core0>°"
                                   , "-L", "30"
                                   , "-H", "75"
                                   , "-l", "lightblue"
                                   , "-n", "#bbc2cf"
                                   , "-h", "#aa4450"] 50

                    -- battery monitor
                    , Run BatteryP       [ "BAT0" ]
                                         [ "--template" , "<fc=#B1DE76><fn=1></fn></fc> <acstatus>"
                                         , "--Low"      , "10"        -- units: %
                                         , "--High"     , "80"        -- units: %
                                         , "--low"      , "#fb4934" -- #ff5555
                                         , "--normal"   , "#bbc2cf"
                                         , "--high"     , "#98be65"

                                         , "--" -- battery specific options
                                                   -- discharging status
                                                   , "-o"   , "<left>% (<timeleft>)"
                                                   -- AC "on" status
                                                   , "-O"   , "<left>% (<fc=#98be65>Charging</fc>)" -- 50fa7b
                                                   -- charged status
                                                   , "-i"   , "<fc=#98be65>Charged</fc>"
                                         ] 50
                    , Run Com "/bin/sh" ["-c", "~/.xmonad/get_volume.sh"]  "myvolume" 5
                    --, Run Com "/bin/sh" ["-c", "cat ~/.xmonad/log.txt | tail -n 1"]  "mylog" 2

                    , Run BufferedPipeReader "mylog"
                      [ (  0, False, "/tmp/xmonad.pipe.base"  )
                      , ( 20, False, "/tmp/xmonad.pipe.log" )
                      ]
                    
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ <fc=#98be65>%mylog%</fc> || <fn=1><fc=#42bcf4></fc> </fn>%myvolume% | %cpu% | %coretemp% | %memory% | %battery% | %dynnetwork% | %date%  |"
       }
