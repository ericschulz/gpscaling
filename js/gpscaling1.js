////////////////////////////////////////////////////////////////////////
//                  JS-CODE FOR Structure Bandits                     //
//                       AUTHOR: ERIC SCHULZ                          //
//                    UCL LONDON,  November 2017                      //
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
//INTIALIZE 
////////////////////////////////////////////////////////////////////////

//data storage ref
var ntrials = 200,//number of trials
    trial=0,//trial counter
    age=0,//age of participant
    gender=0,//gender of particpant
    instcounter=0,//instruction counter
    overallscore=0,//overall score
    correctcollect=[],//collecting the selected position
    timecollect=[],//collection the timestamps
    choicecollect=[],//collecting the chosen functions
    timeInMs=0,//reaction time
    timetotal=0,
    correcttotal=0,
    letter='<input type="image" src="pngs/func',//the letter
    pspecs='.png"  width="400" height="400"'//size of box


//borders for selections
var lambda=permute([2,5])[0];
var N=Array.apply(null, Array(40)).map(function(){return 10});
N=N.concat(Array.apply(null, Array(40)).map(function(){return 20}));
N=N.concat(Array.apply(null, Array(40)).map(function(){return 30}));
N=N.concat(Array.apply(null, Array(40)).map(function(){return 40}));
N=N.concat(Array.apply(null, Array(40)).map(function(){return 50}));
N=permute(N);
var near=Array.apply(null, Array(100)).map(function(){return 30});
near=near.concat(Array.apply(null, Array(100)).map(function(){return 70}));
near=permute(near);
var prop=[];
for (i = 0; i <= 99; i++)  
{
  prop=prop.concat("blue");
  prop=prop.concat("red");
}
prop=permute(prop);

var sim=[];
for (i = 0; i <= 9; i++)  
{
  sim=sim.concat([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]);
}
sim=permute(sim);


var borders=['border="0">'];
//leter boxes and their borders
var input=letter+sim[trial]+'l'+lambda+'n'+N[trial]+'near'+near[trial]+pspecs+borders[trial];
var target=letter+prop[trial]+sim[trial]+'l'+lambda+'n'+N[trial]+'near'+near[trial]+pspecs+borders[trial];

//function to draw the letter boxes into the HTML
function drawpattern(inp)
{
  change('pattern', inp);
}
//do this once at start


////////////////////////////////////////////////////////////////////////
//CREATE HELPER FUNCTIONS
////////////////////////////////////////////////////////////////////////

//function to hide one html div and show another
function clickStart(hide, show)
{
        document.getElementById(hide).style.display ='none' ;
        document.getElementById(show).style.display ='block';
        window.scrollTo(0,0);        
}

//changes inner HTML of div with ID=x to y
function change (x,y)
{
    document.getElementById(x).innerHTML=y;
}

//Hides div with id=x
function hide(x)
{
  document.getElementById(x).style.display='none';
}

//shows div with id=x
function show(x)
{
  document.getElementById(x).style.display='block';
  window.scrollTo(0,0);
}

//permute a list
function permute(o)
{
 for(var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
};


//Display a float to a fixed percision
function toFixed(value, precision) 
{
    var precision = precision || 0,
        power = Math.pow(10, precision),
        absValue = Math.abs(Math.round(value * power)),
        result = (value < 0 ? '-' : '') + String(Math.floor(absValue / power));

    if (precision > 0) {
        var fraction = String(absValue % power),
            padding = new Array(Math.max(precision - fraction.length, 0) + 1).join('0');
        result += '.' + padding + fraction;
    }
    return result;
}


var fullurl = window.location.href;

// extract URL parameters (FROM: https://s3.amazonaws.com/mturk-public/externalHIT_v1.js)
function turkGetParam(name) {
  var regexS = "[\?&]" + name + "=([^&#]*)";
  var regex = new RegExp(regexS);
  var tmpURL = fullurl;
  var results = regex.exec(tmpURL);
  if (results == null) {
    return "";
  } else {
    return results[1];
  }
}

var turkid=0;
function logBonus() 
{
    turkid = turkGetParam('workerId');
 }

////////////////////////////////////////////////////////////////////////
//Instruction Check
////////////////////////////////////////////////////////////////////////
function instructioncheck()
{
    //check if correct answers are provided
    if (document.getElementById('icheck1').checked) {var ch1=1}
    if (document.getElementById('icheck2').checked) {var  ch2 = 1}
    if (document.getElementById('icheck3').checked) {var  ch3 = 1}
    //are all of the correct
    var checksum=ch1+ch2+ch3;
    if (checksum===3){
      //if correct, continue
      inittrial();
      clickStart('page6', 'page7');
      //alert
      alert('Great, you have answered all of the questions correctly. The study will now start.');
    } else{
    	instcounter++;
        //if one or more answers are wrong, raise alert box
        alert('You have answered some of the questions wrong. Please try again.');
        //go back to instructions
        clickStart('page6', 'page3');
        //begintrial();
    }
}

////////////////////////////////////////////////////////////////////////
//Experiment
////////////////////////////////////////////////////////////////////////
var inpinit='<input type="image" src="pngs/init.png"  width="400" height="400">'
var inpbegin='<input type="image" src="pngs/begin.png"  width="400" height="400">'
var inpcor='<input type="image" src="pngs/correct.png"  width="400" height="400">'
var inpwro='<input type="image" src="pngs/incorrect.png"  width="400" height="400">'


var spacepressed = 1;

function inittrial()
{
  spacepressed=0;
  drawpattern(inpbegin);
  change('fingerpos', "");
  //only allowing for one press
  $(document).keypress(function(e) 
  {
    if(e.which == 32 & spacepressed == 0) 
    { 
      spacepressed=1;
      drawpattern(inpinit);
      setTimeout(function(){drawpattern(input);}, 1000);
      setTimeout(function(){begintrial();}, 2000);
    }
  });
}


var returnpressed = 1;

//this function initializes a trial
function begintrial()
{
  drawpattern(target);
  var fingpos='<input type="image" src="pngs/op.png"  width="400" height="100">'
  change('fingerpos', fingpos);
  returnpressed=0;
  //initialize time count
  timeInMs = Date.now()
  //get the pressed key
  $(document).keypress(function(e) 
    {
           //if the key equals A
           if(e.which == 111 & returnpressed == 0) 
                  { 
                    //indicate that something has been pressed          
                    returnpressed=1;
                    //get the time that has passed
                    timeInMs=Date.now()-timeInMs;
                    //call the function for that position
                    myfunc(0);
                  }
            //same spiel if key equals S      
           if(e.which == 112 & returnpressed == 0) 
                  {
                    returnpressed=1;
                    timeInMs=Date.now()-timeInMs;
                    myfunc(1);
                  }
           
            }
      );
}

var correct=0
//funmction that exectutes the bandit
function myfunc(inp)
{
  correct=0;
  if (inp==0 & prop[trial]=="blue"){correct=1;}
  if (inp==1 & prop[trial]=="red"){correct=1;}
  if (correct==1)
  {
    drawpattern(inpcor);
    var timescore=Math.exp(-(converttime(timeInMs)-0.2))*100;
    var feedback="You collected "+ Math.round(timescore)+" time points."
  }
  if (correct==0)
  {
    drawpattern(inpwro);
    var timescore=0;
    var feedback="You collected "+ Math.round(timescore)+" time points."
  }
  change('fingerpos', feedback);
  timetotal=timetotal+timescore;
  timecollect[trial]=timeInMs;
  choicecollect[trial]=inp;
  correctcollect[trial]=correct;
  setTimeout(function(){nexttrial();}, 2000);
}

function converttotal(totalvar)
{
  var out=0;
  if (totalvar=="true"){out=1};
  return(out)
}

function converttime(timevar)
{
  timevar=timevar/1000;
  if (timevar>3){timevar=3};
  return(timevar)
}

function nexttrial()
{

  //check if trials are smaller than the maximum trial number
  if (trial+1<ntrials)
  {
    //increment trial number
    trial++;
    //leter boxes and their borders
    input=letter+sim[trial]+'l'+lambda+'n'+N[trial]+'near'+near[trial]+pspecs+borders[0];
    target=letter+prop[trial]+sim[trial]+'l'+lambda+'n'+N[trial]+'near'+near[trial]+pspecs+borders[0];
    inittrial();
    //to be inserted number of trials left
    var insertt='Number of trials left: '+(ntrials-trial);
    //show on screen
    change('remain', insertt);
    //change ooutcome back to please choose an option    
  } else
  {
    //Otherwise --if blocks exceed total block number, then the experiment is over
    alert("The experiment is over. You will now be directed to the next page.")
    clickStart('page7', 'page8');
    logBonus();
  }
}

////////////////////////////////////////////////////////////////////////
//Demographics & Finish
////////////////////////////////////////////////////////////////////////
//sets the selected gender
function setgender(x)
{
  gender=x;
  return(gender)
}

//sets the selected age
function setage(x)
{
  age=x;
  return(age)
}

function setrecontact(x){
  recontact=x;
  return(recontact)
}

function mysubmit()
{
  
  //change page
  clickStart('page8','page9');
  //claculate number of mined emeralds overall
  var presenttotal='Your average time scores is '+toFixed(timetotal/200,1)+' points.'
  //calculate money earned
  var money =4*(timetotal/20000);
  moneyp=toFixed(money, 2);
  var presentmoney='This equals a bonus of $'+moneyp+'.<br>';
  presentmoney=presentmoney+"The total amount of $"+toFixed(money+2, 2)+" will be transferred to you within the next 3 working days."
  //show score and money
  change('result',presenttotal); 
  change('money',presentmoney);
  //save all created values
  myDataRef.push({money: money, age: age, gender: gender,
                  timecollect: timecollect,timetotal: timetotal, 
                  choicecollect: choicecollect, correctcollect: correctcollect, 
                  N: N, near: near,prop: prop, sim: sim, lambda: lambda,
                  instcounter: instcounter, turkid: turkid, money:money});
 }
////////////////////////////////////////////////////////////////////////
//The END
//////////////////////////////////////////////////////////////////////// 
