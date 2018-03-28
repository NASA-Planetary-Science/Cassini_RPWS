//  dasStd.js
//
// Robert Johnson
// August 15, 2002
//
// 02-09-19 Fix so the correct units are displayed with the plot
//



function onSubmitDas()
{
var sTmp="";
var nTmp;
var das=document.dasStd;
var f=document.CasOpt;
var orbit = new casOrbit();
var dasTm0 = new dasTime();
var dasTm1 = new dasTime();

  // Nix any quotes and replace with spaces
  orbit.tBeg = new String(f.time_start.value);
  orbit.tBeg = orbit.tBeg.replace(/[']+/g,"");
  if(dasTm0.parsetime(orbit.tBeg) == false)
    dasTm0.PopupParseTmErr();
  orbit.tBeg = dasTm0.FormatRPWS();

  orbit.tEnd = new String(f.time_stop.value);
  orbit.tEnd = orbit.tEnd.replace(/[']+/g,"");
  if(dasTm1.parsetime(orbit.tEnd) == false)
    dasTm1.PopupParseTmErr();
  orbit.tEnd = dasTm1.FormatRPWS();

  orbit.Lookup();
  
  das["label(0).value"].value="'" +  orbit.Name + "'";


// display size 
  das["display.isize"].value=dasGetValue(f.disp_xsize);
  das["display.jsize"].value=dasGetValue(f.disp_ysize);

// data set
  das["image(0).dataset"].value="'" + dasGetValue(f.data_set) + "'";

// mime type 
  if(dasGetValue(f.mime_type)=="GIF"){
    das["device_name"].value="'Z'";
    das["gif_mime_header"].value="'" + dasGetValue(f.mime_gif) + "'";
  }
  else if(dasGetValue(f.mime_type)=="PS"){
    das["device_name"].value="'PS'";
    das["gif_mime_header"].value="'" + dasGetValue(f.mime_gif) + "'";
  }
  else{
    das["device_name"].value="'Z'";
    das["gif_mime_header"].value="'image/gif'";
  } 
 
// orbit parameters
  das["axis(0).tca"].value="'" + dasGetValue(f.orbit_parm) + "'";

// data overlay
  das["plot(0).enable"].value=dasGetValue(f.data_overlay[0]); // Fce
  das["plot(1).enable"].value=dasGetValue(f.data_overlay[1]); // FceUT
  das["plot(2).enable"].value=dasGetValue(f.data_overlay[2]); // Flh
  das["plot(3).enable"].value=dasGetValue(f.data_overlay[3]); // Fuh
  das["plot(4).enable"].value=dasGetValue(f.data_overlay[4]); // Fpe
  das["plot(5).enable"].value=dasGetValue(f.data_overlay[5]); // Fuh (LP Den)
 
// plot title
  das["set(1).title"].value="'" + dasGetValue(f.plot_title) + "'";

// time range 
  das["column(0).tleft"].value= "'" + dasGetValue(f.time_start) + "'";
  das["column(0).tright"].value="'" + dasGetValue(f.time_stop) + "'";

// frequency range 
  if(dasGetValue(f.freq_min_units)=="MHz")
    das["row(0).ybottom"].value=dasGetValue(f.freq_min)*1.0E6;
  else if(dasGetValue(f.freq_min_units)=="KHz")
    das["row(0).ybottom"].value=dasGetValue(f.freq_min)*1.0E3;
  else
    das["row(0).ybottom"].value=dasGetValue(f.freq_min)*1.0;
  if(dasGetValue(f.freq_max_units)=="MHz")
    das["row(0).ytop"].value=dasGetValue(f.freq_max)*1.0E6;
  else if(dasGetValue(f.freq_max_units)=="KHz")
    das["row(0).ytop"].value=dasGetValue(f.freq_max)*1.0E3;
  else
    das["row(0).ytop"].value=dasGetValue(f.freq_max)*1.0;
  das["row(0).log"].value=dasGetValue(f.freq_log);

// amplitude range 
  das["colormap(0).zlow"].value=dasGetValue(f.amp_min);
  das["colormap(0).zhigh"].value=dasGetValue(f.amp_max);
  das["colormap(0).log"].value=dasGetValue(f.amp_log);
  if(dasGetValue(f.amp_gray)=="GRAY")
    das["gray scale"].value=1;
  else
    das["gray scale"].value=0;

// background division - set amplitude units; 3 cases: das,rdr,das&rdr
  if(dasGetValue(f.bgd)){
    if(dasGetValue(f.amp_log)==0)  // linear color bar
      das["axis(1).y.title"].value="'Normalized to Background (" + 
                                 dasGetValue(f.bgd_per) + "%)'";
    else
      das["axis(1).y.title"].value="'dB Above Background (" + 
                                 dasGetValue(f.bgd_per) + "%)'";
    if(dasGetValue(f.bgd_mode)=="rdr"){  // reader only
      das["image(0).bgd"].value=0;
      das["colormap(0).db"].value=0;
      das["colormap(0).zlow"].value=dasGetValue(f.amp_min);
      if((dasGetValue(f.amp_min)<=0) && (dasGetValue(f.amp_log)==1))
        das["colormap(0).zlow"].value=1;  
    } 
    else{  // das performs a background division
      das["image(0).bgd"].value=dasGetValue(f.bgd_per);
      das["colormap(0).db"].value=1;
    }
  }
  else{
    das["image(0).bgd"].value=0;
    das["colormap(0).db"].value=0;
    das["axis(1).y.title"].value=dasGetAmpUnits();
  }

  // Pass user 
  das["image(0).query"].value=dasGetCmdArg();

return false;
}



function onNext(bDirection)
{
var dDiff,sTmp;
var dasTm0 = new dasTime();
var dasTm1 = new dasTime();
var f=document.CasOpt;

  // Nix any quotes and replace with spaces
  sTmp = new String(f.time_start.value);
  sTmp = sTmp.replace(/[']+/g,"");
  if(dasTm0.parsetime(sTmp) == false)
    dasTm0.PopupParseTmErr();

  sTmp = new String(f.time_stop.value);
  sTmp = sTmp.replace(/[']+/g,"");
  if(dasTm1.parsetime(sTmp) == false)
    dasTm1.PopupParseTmErr();

  if(bDirection == true){  // forward in time
    dDiff = dasTm1.ttime() - dasTm0.ttime();  // normalizes and returns seconds
    dasTm0.nSecond += 2.0*dDiff;  // it be nice to overload the + operator
    dasTm0.tnorm();
  }
  else{  // backwards in time
    dDiff = dasTm1.ttime() - dasTm0.ttime();  // normalizes and returns seconds
    dasTm1.nSecond -= 2.0*dDiff;  // it be nice to overload the + operator
    dasTm1.tnorm();
  }
  
  // Update time range text fields
  f.time_start.value=dasTm1.FormatRPWS();
  f.time_stop.value=dasTm0.FormatRPWS();

  onSubmitDas();

return;
}



function dasGetAmpUnits()
{
var bAnt,bBnd,bEle,bMag;
var bEle=false,bMag=false;
var sUnits="",sTmp="";

  sTmp=dasGetLfdrBndAnt();
  sTmp+=dasGetMfdrBndAnt();
  sTmp+=dasGetMfrBndAnt();
  sTmp+=dasGetHfrBndAnt();
  if(sTmp.search(/E/) != -1)  bEle=true; 
  if(sTmp.search(/Bx/) != -1)  bMag=true; 
  if(sTmp.search(/By/) != -1)  bMag=true; 
  if(sTmp.search(/Bz/) != -1)  bMag=true; 

  //electric das["axis(1).y.title"]="'V!E2!N m!E-2!N Hz!E-1!N'"
  //magnetic das["axis(1).y.title"]="'nT!E2 !NHz!E-1 N!'"

  if(bEle==true && bMag==true)  
    sUnits="'" + "V!E2!N m!E-2 !NHz!E-1 !N or nT!E2 !NHz!E-1 !N" + "'";
  else if(bEle==true)  
    sUnits="'" + "V!E2 !Nm!E-2 !NHz!E-1 !N" + "'";
  else if(bMag==true)
    sUnits="'" + "nT!E2 !NHz!E-1 !N" + "'";
  else 
    sUnits="'" + "V!E2 !Nm!E-2 !NHz!E-1 !N" + "'";

return sUnits;
}



function dasGetValue(e)
{

  if(e.type=="select-one"){
    for(var i=0;i<e.options.length;i++)
      if(e.options[i].selected)
        value=e.options[i].value;
  }
  else if(e.type=="text"){
    value=e.value;
  }
  else if(e.type=="checkbox"){
    if(e.checked)  
      value=e.value;
    else
      value=0;
  }
  else if( e[0].type=="radio" ){
    for(var i=0;i<e.length;i++)
      if(e[i].checked)
        value=e[i].value;
  }
  else{
    sTmp="error: length=" + e.length + " name=" + e.name;
    alert(sTmp);
  }

return value;
}


function dasGetCmdArg()
{
var fopt=document.CasOpt;
var fant=document.CasAnt;
var bAnt=false,bBnd=false;
var sTmp="",sCmd="";
var bLfr=false,bMfdr=false,bMfr=false,bHfr=false;


  sTmp=dasGetLfdrBndAnt();
  if(sTmp != "") 
    sCmd+="-lfdr " + sTmp + " ";
  sTmp=dasGetMfdrBndAnt();
  if(sTmp != "") 
    sCmd+="-mfdr " + sTmp + " ";
  sTmp=dasGetMfrBndAnt();
  if(sTmp != "") 
    sCmd+="-mfr " + sTmp + " ";
  sTmp=dasGetHfrBndAnt();
  if(sTmp != "") 
    sCmd+="-hfr " + sTmp + " ";

  // data filtering options: 0=hfr pwr supply noise, hfr sounder, lp raw sweep  
  for(var i=0;i<fant.dfo.length;i++){
    if(fant.dfo[i].checked==true)  
      sCmd+=fant.dfo[i].value + " "; 
  }

  // check for mfr/hfr overlaps
  for(var i=0;i<fant.mfr3_hfrA.length;i++){
    if(fant.mfr3_hfrA[i].selected==true)  
      sCmd+=fant.mfr3_hfrA[i].value + " "; 
  }
  // check for mfr/hfr overlaps
  for(var i=0;i<fant.hfrC_hfrHF1.length;i++){
    if(fant.hfrC_hfrHF1[i].selected==true)  
      sCmd+=fant.hfrC_hfrHF1[i].value + " "; 
  }
  // mfr band 2 replacement with mfdr
  if(fant.mfr_mfdr.checked==true)   
    sCmd+=fant.mfr_mfdr.value + " "; 

  // Processing Options
  if(fopt.ant_res.checked==true){
    sCmd+=dasGetValue(fopt.ant_res);
  }
  // arguments for dasort, background division by mode
  if(fopt.bgd.checked==true){
    if(dasGetValue(fopt.bgd_mode)!="das")
      sCmd+="-b " + dasGetValue(fopt.bgd_per) + " ";
  }

  // use day long interval
  if(fopt.bgd_day.checked==true){
    sCmd+=dasGetValue(fopt.bgd_day) + " ";
  }


return sCmd;  
}



function dasGetLfdrBndAnt()
{
var sBnd="",sAnt="",sTmp="";
var fBnd=document.CasAnt.lfdr_bnd;
var fAnt=document.CasAnt.lfdr_ant;

  if(fBnd.checked==true)
    sBnd="L";
  for(var i=0;i<fAnt.length;i++){
    if(fAnt[i].checked==true) 
      sAnt+=fAnt[i].value;  
  }
  if(sBnd!="" && sAnt!="")
    sTmp=sAnt;  // no band information

return sTmp;
}

function dasGetMfdrBndAnt()
{
var sBnd="",sAnt="",sTmp="";
var fBnd=document.CasAnt.mfdr_bnd;
var fAnt=document.CasAnt.mfdr_ant;

  if(fBnd.checked==true)
    sBnd="H";
  for(var i=0;i<fAnt.length;i++){
    if(fAnt[i].checked==true) 
      sAnt+=fAnt[i].value;  
  }
  if(sBnd!="" && sAnt!="")
    sTmp=sAnt;  // no band information

return sTmp;
}

function dasGetMfrBndAnt()
{
var sBnd="",sAnt="",sTmp="";
var fBnd=document.CasAnt.mfr_bnd;
var fAnt=document.CasAnt.mfr_ant;

  for(var i=0;i<fBnd.length;i++){
    if(fBnd[i].checked==true)
      sBnd+=fBnd[i].value;  
  }
  for(var i=0;i<fAnt.length;i++){
    if(fAnt[i].checked==true) 
      sAnt+=fAnt[i].value;  
  }
  if(sBnd!="" && sAnt!="")
    sTmp=sBnd + sAnt;

return sTmp;
}

function dasGetHfrBndAnt()
{
var sBnd="",sAnt="",sTmp="";
var fBnd=document.CasAnt.hfr_bnd;
var fAnt=document.CasAnt.hfr_ant;

  for(var i=0;i<fBnd.length;i++){
    if(fBnd[i].checked==true)
      sBnd+=fBnd[i].value;  
  }
  for(var i=0;i<fAnt.length;i++){
    if(fAnt[i].checked==true) 
      sAnt+=fAnt[i].value;  
  }
  if(sBnd!="" && sAnt!="")
    sTmp=sBnd + sAnt;

return sTmp;
}
