/* myTime:
   a simple data time writer
  
   use with 'myDew' e.g 'myDew("day", 16)
  
   will take the currant time and date with day 16
   => 2011-03-16 15:44 


   readTextFile"archive/btc.csv"   

   motR("year", 2012, 20);

    => make a list of lenght 20      */



function strings(l){
   return l.toString()
};
// need to addd zero when g<10
// g:: provides of 'getDate' the 2ND digit of day e.g 09 or 10..  
function someZero(g){
   if (g < 10){
       return "0"+g.toString() }
   else {
       return g.toString()    }
  };
function myDateRAW() {
  var d = new Date();
  var n = strings((d.getFullYear())-10);
  var o = someZero(d.getMonth());
  var p = someZero(d.getDate());
  var q = someZero(d.getUTCHours()+1);
  var r = someZero((d.getMinutes()));
  document.getElementById("demo").innerHTML = n+"-"+o+"-"+p+" "+q+":"+r;
}  


// same as above but export string to array 
// same as above but export string to array 
function myDateWORK() {
  var d = new Date();
  var n = strings((d.getFullYear())-10);
  var o = someZero(d.getMonth());
  var p = someZero(d.getDate());
  var q = someZero(d.getUTCHours()+1);
  var r = someZero((d.getMinutes()));
  return (n+","+o+","+p+","+q+","+r);
} 



function monthChoose(z){
 var myDateWork = myDateWORK().split(",");
 if (z=="month" && (myDateWork)[1]== 01 || (myDateWork)[1]== 03 || (myDateWork)[1]== 05 || (myDateWork)[1]== 07 || (myDateWork)[1]== 08 || (myDateWork)[1]== 10 || (myDateWork)[1]== 12){
     return 31
  }
 else if (z=="month" && (myDateWork)[1]== 02 ){
     return 29
  }
  else if (z=="month" && (myDateWork)[1]== 02 ){
     return 28
  }
  else if (z=="month" && (myDateWork)[1]== 04  || (myDateWork)[1]== 06 || (myDateWork)[1]== 09  || (myDateWork)[1]== 11 ){
     return 30
  }
  
  
}
  
  function chodeWork(z){
  if (z=="minute"){
     return 4}
  if (z=="hours"){
     return 3}
  if (z=="day"){
     return 2}
  if (z=="month"){
     return 1}
  if (z=="year"){
     return 0}
  }
 


function sortER(z){
 var myDate = myDateWORK().split(",");
 if (z=="year"){
         return myDate[0]}
 else if (z=="month"){
         return myDate[1]}
 else if (z=="day"){
         return myDate[2]}
 else if (z=="hour"){
         return myDate[3]}
 else if (z=="minute"){
         return myDate[4]}
 
}
function condChode(z, inP){
  var myDateWork = myDateWORK().split(",");
  var inpUU = Number(myDateWork[chodeWork(z)]);
  if (inP>60 && z=="minute") {
      return ((myDateWork)[3])+1
   }
  else if (inP>23 && z == "hour") {
      return ((myDateWork)[2])+1
   } 
   
   else if (inP>(monthChoose("month")) && z == "month") {
      return ((myDateWork)[1])+1
   } 
   else if (inP>12 && z == "month" && monthChoose("month")) {
       return ((myDateWork)[0])+1
   } 
   else{
       return inpUU}
      
  }

// aValu :: a value 
function myDew(z, aValu) {
 var myDate = myDateWORK().split(",");
 
 myDate[chodeWork(z)] = strings(aValu); // select year or month ..change        
 var o = myDate[0] ;//Number(chondChode(z));
 var p = myDate[1] ;
 var q = myDate[2] ;
 var r = myDate[3] ;
 var s = myDate[4] ;
 //strings(someZero(Number(condChode(z))));
 //var p = someZero(condChode(z));
 //var q = strings(condChode(z)+10);
 //var r = strings(condChode(z));
  return o+"-"+p+"-"+q+" "+r+":"+s;
}




function myFunction() {
  var d = new Date();
  var n = strings(d.getFullYear());
  var o = someZero(d.getMonth());
  var p = someZero(d.getDate());
  var q = strings(d.getUTCHours()+1);
  var r = strings(d.getMinutes());
  document.getElementById("demo").innerHTML = n+"-"+o+"-"+p+" "+q+":"+r;
}

function readTextFile(file)
{
    var rawFile = new XMLHttpRequest();
    rawFile.open("GET", file, false);
    rawFile.onreadystatechange = function ()
    {
        if(rawFile.readyState === 4)
        {
            if(rawFile.status === 200 || rawFile.status == 0)
            {
                var allText = rawFile.responseText;
                alert(allText);
            }
        }
    }
    rawFile.send(null);
}

// seleC :: "year" or "month" ...
// motoR :: Num String, e.g  "2012" will insert "2012" into selected spot
// solonG :: e.g. "20" wil generate list length 20
function motR(seleC, motoR, solonG){
   var text = "";
   var motor = Number(solonG);
   var counT = Number(motoR);
   var i=0;
   while (i<motor) {
	var rekeN = i+counT;
	var uiT = rekeN.toString();
        text += "" + (someZero(i))+ " "+myDew(seleC,uiT)+"\n";
        i++;
                           	}
        return text;	 
   }


