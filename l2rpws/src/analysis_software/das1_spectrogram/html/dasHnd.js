



// Change The Plot Title Based on the Antenna and Band Selection
function onTitleAuto(fAnt,fStd)
{
var bAnt,bBnd;
var sTitle="Pds1.3 ",sTmp="";
var fant,fbnd;
var titpre1 = /cassini\/rpws\/raj\/RPWSspec(\S*)/;
var titpre2= /cassini\/rpws\/raj\/RPWS(\S*)/;
var titpre3= /cassini\/rpws\/tfa\/RPWS(\S*)/;

  if(fStd.title_auto.checked==false)
    return;

// prefix title with reader raj
  sTitle=dasGetValue(fOpt.data_set);
  result=sTitle.match(titpre1);
  if(result!=null){
    sTitle=result[1] + " ";
  }
  result=sTitle.match(titpre2);
  if(result!=null){
    sTitle=result[1] + " ";
  }
  result=sTitle.match(titpre3);
  if(result!=null){
    sTitle=result[1] + " ";
  }

  if((sTmp=dasGetLfdrBndAnt())!="")
    sTitle+="Lfdr " + sTmp + ", ";
  if((sTmp=dasGetMfdrBndAnt())!="")
    sTitle+="Mfdr " + sTmp + ", ";
  if((sTmp=dasGetMfrBndAnt())!="")
    sTitle+="Mfr " + sTmp + ", ";
  if((sTmp=dasGetHfrBndAnt())!="")
    sTitle+="Hfr " + sTmp + ", ";
  sTitle=sTitle.replace(/,\s*$/g,"");

// alert(sTitle);

  fOpt.plot_title.value=sTitle;

return;
}



// Change Frequency Range Based on Band Selection
function onFrqAuto(fAnt,fStd)
{
var nMax,nMin;
var lfdr=fAnt.lfdr_bnd,mfdr=fAnt.mfdr_bnd,mfr=fAnt.mfr_bnd,hfr=fAnt.hfr_bnd;

  if(fStd.freq_auto.checked==false)
    return;

  // Find Maximum Frequency Value
  if(lfdr.checked==true)    nMax=      25.0;
  if(mfr[0].checked==true)  nMax=     169.0;
  if(mfr[1].checked==true)  nMax=    1470.0;
  if(mfdr.checked==true)    nMax=    2500.0;
  if(mfr[2].checked==true)  nMax=   11800.0;
  if(hfr[0].checked==true)  nMax=   15900.0;
  if(hfr[1].checked==true)  nMax=   71300.0;
  if(hfr[2].checked==true)  nMax=  320500.0;
  if(hfr[3].checked==true)  nMax= 4200000.0;
  if(hfr[4].checked==true)  nMax=16100000.0;

  // Find Minium Frequency Value
  if(hfr[4].checked==true)  nMin=1000000.0;
  if(hfr[3].checked==true)  nMin= 300000.0;
  if(hfr[2].checked==true)  nMin=  74600.0;
  if(hfr[1].checked==true)  nMin=  16500.0;
  if(hfr[0].checked==true)  nMin=   3600.0;
  if(mfr[2].checked==true)  nMin=   1536.0;
  if(mfr[1].checked==true)  nMin=    192.0;
  if(mfr[0].checked==true)  nMin=     23.0;
  if(mfdr.checked==true)    nMin=     10.0;
  if(lfdr.checked==true)    nMin=      2.0;

  // Update the text areas and units
  if((nMin/1.0E6)>1.0){
    nMin/=1.0E6;
    fStd.freq_min_units.selectedIndex=2;
  }
  else if((nMin/1.0E3)>1.0){
    nMin/=1.0E3;
    fStd.freq_min_units.selectedIndex=1;
  }
  else{
    fStd.freq_min_units.selectedIndex=0;
  }
  fStd.freq_min.value=nMin;

  if((nMax/1.0E6)>1.0){
    nMax/=1.0E6;
    fStd.freq_max_units.selectedIndex=2;
  }
  else if((nMax/1.0E3)>1.0){
    nMax/=1.0E3;
    fStd.freq_max_units.selectedIndex=1;
  }
  else{
    fStd.freq_max_units.selectedIndex=0;
  }
  fStd.freq_max.value=nMax;


}



function onAmpAuto(e)
{
var p=document.CasOpt;

  if(p.amp_auto.checked==false)
    return;

  if( e.name=="bgd" && e.checked==true ){
    p.amp_log.checked=true;
    p.amp_min.value="0";
    p.amp_max.value="15";
  }
  else{
    p.amp_log.checked=true;
    p.amp_min.value="1.0E-18";
    p.amp_max.value="1.0E-9";
  }

return;
}



function ant_handler()
{
var fAnt=document.CasAnt,fOpt=document.CasOpt;

  onTitleAuto(fAnt,fOpt);

return;
}



function bnd_handler()
{
var fAnt=document.CasAnt,fOpt=document.CasOpt;

  onFrqAuto(fAnt,fOpt);
  onTitleAuto(fAnt,fOpt);

return;
}

function mfdr_handler()
{
var fAnt=document.CasAnt,fOpt=document.CasOpt;

  if(fAnt.mfr_mfdr.checked==true){
    fAnt.mfr_bnd[1].checked=false;
    fAnt.mfdr_bnd.checked=true;
  }
  else{
    fAnt.mfr_bnd[1].checked=true;
    fAnt.mfdr_bnd.checked=false;
  }

  onTitleAuto(fAnt,fOpt);

return;
}

