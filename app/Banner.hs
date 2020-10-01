{-# LANGUAGE TemplateHaskell #-}
module Banner
    ( banner
    ) where

import Distribution.PackageDescription.TH

banner :: Text
banner = "         ▄              ▄                                                    \n\
         \        ▌▒█           ▄▀▒▌                                                     \n\
         \        ▌▒▒█        ▄▀▒▒▒▐                                                     \n\
         \       ▐▄█▒▒▀▀▀▀▄▄▄▀▒▒▒▒▒▐                                                     \n\
         \     ▄▄▀▒▒▒▒▒▒▒▒▒▒▒█▒▒▄█▒▐                                                     \n\
         \   ▄▀▒▒▒░░░▒▒▒░░░▒▒▒▀██▀▒▌                   " <> namever <> "\n\
         \  ▐▒▒▒▄▄▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▀▄▒▌                                                    \n\
         \  ▌░░▌█▀▒▒▒▒▒▄▀█▄▒▒▒▒▒▒▒█▒▐           __     _______ ____  _                   \n\
         \ ▐░░░▒▒▒▒▒▒▒▒▌██▀▒▒░░░▒▒▒▀▄▌          \\ \\   / /  ___|  _ \\| |               \n\
         \ ▌░▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░▒▒▒▒▌           \\ \\ / /| |_  | | | | |                \n\
         \▌▒▒▒▄██▄▒▒▒▒▒▒▒▒░░░░░░░░▒▒▒▐            \\ V / |  _| | |_| | |___              \n\
         \▐▒▒▐▄█▄█▌▒▒▒▒▒▒▒▒▒▒░▒░▒░▒▒▒▒▌            \\_/  |_|   |____/|_____|             \n\
         \▐▒▒▐▀▐▀▒▒▒▒▒▒▒▒▒▒▒▒▒░▒░▒░▒▒▐                                                   \n\
         \ ▌▒▒▀▄▄▄▄▄▄▀▒▒▒▒▒▒▒░▒░▒░▒▒▒▌         Very Factory Description Language         \n\
         \ ▐▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░▒░▒▒▄▒▒▐                                                    \n\
         \  ▀▄▒▒▒▒▒▒▒▒▒▒▒▒▒░▒░▒▄▒▒▒▒▌      Very Factory                                  \n\
         \    ▀▄▒▒▒▒▒▒▒▒▒▒▄▄▄▀▒▒▒▒▄▀                                   Such Production   \n\
         \      ▀▄▄▄▄▄▄▀▀▀▒▒▒▒▒▄▄▀                        Much Wow                       \n\
         \         ▀▀▀▀▀▀▀▀▀▀▀▀                                                          \n"
  where
    ver = $(packageVariable (pkgVersion . package))
    name = $(packageVariable (pkgName . package))
    namever = name <> "-" <> ver
