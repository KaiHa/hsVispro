# hsVispro
Haskell library to access a Vispro/VisTwo io-base and registry server.

## What is Vispro?
Vispro (or VisTwo) is a proprietary SCADA system, developed by the
[Visual Systems Automation GmbH](http://vsys.de/index.php?menu=downloadvispro).

## What does this library?
This Haskell library is a wrapper around the C libraries libVisIob and libVisReg.

With the module `Vis.Iob` you can access the Vispro io-base server (iob.srv).
The iob.srv contains the variable process data. 

With the module `Vis.Reg` you can access the Vispro registry server (reg.srv).
The reg.srv contains the more or less static engineering data.
