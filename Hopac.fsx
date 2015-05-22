// 0. You can skip down to 2. if you already have the packages
open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

if not (File.Exists "paket.exe") then
  let url = "https://github.com/fsprojects/Paket/releases/download/1.5.0/paket.exe"
  use wc = new Net.WebClient()
  let tmp = Path.GetTempFileName()
  wc.DownloadFile(url, tmp)
  File.Move(tmp,Path.GetFileName url)
 
// Step 1. Resolve and install the packages
#r "paket.exe"
 
Paket.Dependencies.Install """
source https://nuget.org/api/v2
nuget Hopac
nuget Hopac.Extras
""";;
 
// Step 2. Use the packages

#I "packages/Hopac/lib/net45";;
#I "packages/Hopac.Extras/lib/net45";;

#r "Hopac.Core.dll" ;;
#r "Hopac.dll" ;;
#r "Hopac.Extras.dll" ;;
#r "Hopac.Experimental.dll" ;;
#r "Hopac.Platform.dll" ;;

open System ;;
open Hopac.Experimental ;;
open Hopac.Extensions ;;
open Hopac.Extras ;;
open Hopac.Timer.Global ;;
open Hopac.Alt.Infixes ;;
open Hopac.Job.Infixes ;;
open Hopac.Infixes ;;
open Hopac ;;
