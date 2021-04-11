/*  classic functions that are similar to
    other programming langues  exports:

      arrayToObject  ;    [[[x1, 15.4],[x2, 0.4],[x3, 4.55]]]
                      =>  


      myCONCAT(inpuT)     ; e.g  inpuT = [[["Emil", "Tobias", "Linus"]]]

     aMAP(inpuT)          ; e.g inpuT = ["1","2","3"]
*/


/* example for array to object; 1 o 2 : array */

// var dBLitoSolu = [["fl1","40.59405940594059"],["fl2","64.71615720524018"],["fl3","29.616724738675956"],["fl4","29.12280701754386"],["fl5","39.603960396039604"],["fl6","22.60536398467433"]];


/* array to object; 2 o 2: [Array] ->  {Array}
                   call with key = e.g "fl1"  (see 1 o 2)
                                               */ 
const convertArrayToObject = (array, key) => {
  const initialValue = {};
  return array.reduce((obj, item) => {
    return {
      ...obj,
      [item[key]]: item,
    };
  }, initialValue);
};

// entrY ::  [[( String  ,  String )]]     =>  Object one of [Object] 
function entries(entrY){
       var geL = entrY[0];
       for (var j=0; j<geL.length; j++) {
	   var offntry =  (entrY[j]);
           var starT = new Map(offntry);   // [
           return Object.fromEntries(starT)
	     }
        };

// inpuT: e.g [[["Emil", "Tobias", "Linus"]]];
function myCONCAT(inpuT){
   var children = inpuT.concat();
         return children};

// function does [String] to [Float] 
//            a str array to a float array
function aMAP(fomapper){
	 var foprepAr = (""+fomapper+"").split("\n");
	 return foprepAr.map(Number)}
