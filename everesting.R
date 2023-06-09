# Data prep ####

library(data.table)
library(magrittr)
library(highcharter)

rl <- read.csv('ridelog.csv') |> data.table()
rl[,turn := .I]

#shift lowest observation towards 0 from 1
rl <- melt(rl,id.vars = 'turn')
rl[,value := value -.5]
rl <- dcast(rl,formula = turn ~ variable)

#smooth series using loess
span <- .16
rl[,overall:= loess(data = rl, formula = overall ~ turn, span = span) |> predict() ]
rl[,pain   := loess(data = rl, formula = pain    ~ turn, span = span) |> predict() ]
rl[,sleep  := loess(data = rl, formula = sleep   ~ turn, span = span) |> predict() ]
rl[,quit   := loess(data = rl, formula = quit    ~ turn, span = span) |> predict() ]
rl[,muscle := loess(data = rl, formula = muscle  ~ turn, span = span) |> predict() ]
rl[,stomach:= loess(data = rl, formula = stomach ~ turn, span = span) |> predict() ]
rl[,overall:=NULL]
rl <- melt(rl,id.vars = 'turn')

#add stories to the data to be used as popups on the chart ("tooltips")
rl[,story := ""]
rl[variable=='pain', story := 
    "Lower back pain was the only significant issue to deal with during the 1st half of my ride. 
    It was an alarming signal because of its early occurence. &#128556 
    I tried to move my waist in all directions during the ride and also strech it off-the-bike after each turn. 
    It was very surprising that the pain slowly faded away and never came back. 
    Pain I had to deal with at later stages was quite usual (e.g. butt, shoulders) 
    and never grew so big to actually endanger my finish. &#128578 "]
rl[variable=='muscle', story :=
    "I read somewhere that Everesting is <i>a great opportunity to see what's really left in the tank</i>.  
    It is. And it turns out there is a lot. This was by far my longest effort in the recent years and I was not sure
    I was able to complete it in one push. 
    Still after 20+ hours of riding uphill I never felt that I am so tired phisycally I needed to quit. 
    Homo sapiens IS the king of endurance after all. &#128170 &#128526"]
rl[variable=="quit", story := 
    "By the end of my Basecamp (=half everesting) training ride that I did some weeks before the big event 
    I was so worn down mentally that I actually convinced myslef that I will not be able to finish the full distance, 
    and this whole challenge was a stupid idea. &#128517 Only a few days after this I was back again planning
    my Everesting. &#128514 Dealing with this was my biggest fear prior the ride. My spirit was quite high during 
    the 1st half but when things got really ugly during the night dealing with negative thougts was a big challenge. 
    The speech I repeated to myself and that actually helped was this: <br> <i> I invested so much time into it so far 
    (preparations + riding for 15+ hours straight now) that quitting would be a ridiculous waste of effort!</i> &#129299"]
rl[variable=="sleep", story:=
    "Unluckily I never sleep well before big events and this is the reason I started my ride quite sleepy at 7 am. 
    But this was never going to be a big issue... 
    Even though I tried to be moderate with coffein during the day I felt it had no effect on me during the night. 
    Sleepiness and mental fatigue hit me really hard after twilight. Coffee, music, watching movies (yes I did that 
    by attaching my phone to the handlebar &#128513) did not helped much. I had 2 major meltdowns during the night when 
    I felt I could not carry on. My trick was to convince myself that the day was over by taking a relatively long rest 
    (30-45 min) in the car and from then on a new day starts like any other only a bit early. &#128553 
    When the sun came up and finish was within reach (= like 6 hours only &#129315) most of my problems reduced significantly 
    and I was able to actually pick up speed. &#128170 "]
rl[variable=="stomach", story:=
    "Gels and bars work well as supplements but for an effort this big I need real food as main fuel. 
    Same goes to iso drinks: after a while my body cannot accept it and I need to shuffle between tea, coke, juice 
    and eventually nothing but plain water. For this reason I am a big believer of eating mineral tabs during long rides. 
    This concept worked very well in the 1st 12 hours then my stomach became very upset and it 
    never actually recovered until the end. Forcing fuel down my throat and continue in this condition 
    was the toughest challenge I had &#128534 ... Throwing out my pad-thai during the night because ants invaded it inside my car 
    was also a very low and memorable moment &#128512"]

#rename variables to be more informative
rl[,variable := as.character(variable)]
rl[variable == "pain"   ,variable := "Pain"]
rl[variable == "sleep"  ,variable := "Sleepiness"]
rl[variable == "quit"   ,variable := "Voice in my head yelling to quit"]
rl[variable == "muscle" ,variable := "Muscle soreness"]
rl[variable == "stomach",variable := "Upset stomach"]
rl[variable == "sleep"  ,variable := "Sleepiness"]

# Plot ####

#adjust this theme slightly to be more appealing for the eye
thm <- hc_theme_merge(
  hc_theme_darkunica(),
  hc_theme(
    tooltip = list(
      style = list(
        fontFamily = "Oswald",
        fontWeight = "normal"
      )
    ),
    legend = list(
      itemStyle = list(
        fontFamily = "Oswald",
        fontWeight = "normal"
      )
    ),
    xAxis = list(title = list(style = list(color = "#E0E0E3"))) 
  )
)

#generate streamgraph chart
hchart(rl, 
       "streamgraph", 
       style = list(fontFamily = "Oswald"),
       hcaes(turn, value, group = variable),
       tooltip = list(headerFormat = "", #remove header
                      pointFormat = "{point.story}")
       ) |> 
  hc_yAxis(visible = FALSE) |>
  hc_xAxis(title = list(text = "<b>DISTANCE </b> (Number of climbs up to Janos-hegy)")) |>
  hc_title(text = "<b>My Everesting Journey</b>") |>
  hc_subtitle(text = "The thicker the stream the more difficult it was to carry on with my ride") |>
  hc_credits(enabled = TRUE, text = "https://balint-komjati.hu/") |>
  hc_add_theme(thm) -> strmgrph

strmgrph
