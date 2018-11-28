## from https://gist.github.com/statisfactions/1912899
ggmidi <- function(title="R-created Midi", bpm=780){
  ## Creates empty "midi" object that tracks can be inserted into
  
  old <- options(scipen = 7) ## avoids writing anything here in scientific notation
  on.exit(options(scipen = old$scipen))
  
  ## Create new data.frame and add headers
  newdf <- data.frame(matrix(nrow=7,ncol=7))
  names(newdf) <- c("tracknum", "time", "type","channel", "note", "velocity", "tempoonly")
  newdf[1,] <- c(0,0, "Header", 1, 1, 480, NA)
  newdf[2,] <- c(1,0, "Start_track", NA, NA, NA, NA)
  newdf[3,] <- c(1,0, "Title_t", title, NA, NA, NA)
  newdf[4,] <- c(1,0, "Time_signature", 4, 4, 24, 8)
  newdf[5,] <- c(1,0, "Tempo", 60000000/bpm, NA,NA,NA)
  newdf[6,] <- c(1,0, "End_track", NA, NA, NA, NA)  
  newdf[7,] <- c(0,0, "End_of_file", NA, NA, NA, NA)
  
  ## Set class and attributes
  class(newdf) <- c("midi", "data.frame")
  attributes(newdf)$istrack <- FALSE
  attributes(newdf)$percussion <- FALSE
  attributes(newdf)$maxtrack <- 0
  attributes(newdf)$maxchannel <- 0
  newdf
}

audio_note <- function(beat, durs, notes, veloc, program = 1, percussion = FALSE) {
  notes <- round(((notes-min(notes))*72/(max(notes)-min(notes))-36) + 60, 0)
  beat <- 0:(length(notes)-1)
  
  tracknum <- rep(NA,length(beat))
  score <- data.frame(tracknum)
  score$starttime <- beat
  score$channel <- NA
  score$note <- notes
  score$velocity <- veloc
  score$endtime <- score$starttime + durs
  
  ## Change times to MIDI quarter note
  score$starttime <- score$starttime * 480
  score$endtime <- score$endtime * 480
  
  ons <- score[, setdiff(names(score), "endtime")]
  ons$time <- ons$starttime
  ons$type <- "Note_on_c"
  ons <- ons[, c("tracknum", "time", "type","channel", "note", "velocity")]
  
  offs <- score[, setdiff(names(score), "starttime")]
  offs$time <- offs$endtime
  offs$type <- "Note_off_c"
  offs$velocity <- 0
  offs <- offs[, c("tracknum", "time", "type","channel", "note", "velocity")]
  
  all <- rbind(ons, offs)
  all <- all[order(all$time, all$type),]
  all$tempoonly <- NA
  
  begintrack <- c(NA, 0, "Start_track", NA, NA, NA, NA)
  chooseinstrument <- c(NA, 0, "Program_c", NA, program - 1, NA,NA)
  endtrack <- c(NA, max(all$time), "End_track", NA, NA, NA, NA)
  fulltrack <- rbind(begintrack, chooseinstrument, all, endtrack)
  
  class(fulltrack) <- c("midi", "data.frame")
  attributes(fulltrack)$istrack <- TRUE
  attributes(fulltrack)$percussion <- percussion
  fulltrack
}

scale_tempo <- function(bpm=120) {
  class(bpm) <- c("midi", "numeric")
  attributes(bpm)$istrack <- FALSE
  bpm
}

"+.midi" <- function(midiOld, midiTrack, ...) {
  if(attributes(midiTrack)$istrack){
    ## Add a new midi track (first argument) into an existing one (second)
    ## Channel numbering starts at 11 to avoid any confusion with percussion tracks
    ## which in MIDI are always channel 10.
    if(attributes(midiOld)$percussion) {
      channelnum <- 10
    } else {
      channelnum <- attributes(midiOld)$maxchannel + 1
      tracknum <- attributes(midiOld)$maxtrack + 1
    }
    midiOld[1,5] <- as.numeric(midiOld[1,5])+1
    midiTrack$tracknum <- tracknum
    midiTrack$channel <- channelnum
    midiNew <- rbind(midiOld[-nrow(midiOld),], midiTrack, midiOld[nrow(midiOld),])
    ##
    class(midiNew) <- c("midi", "data.frame")
    attributes(midiNew)$istrack <- FALSE
    attributes(midiNew)$percussion <- FALSE
    attributes(midiNew)$maxtrack <- attributes(midiOld)$maxchannel + 1
    attributes(midiNew)$maxchannel <- attributes(midiOld)$maxtrack + 1
  } else {
    midiNew <- midiOld
    midiNew[5,4] <- 60000000/midiTrack
  }    
  midiNew
}

print.midi <- function(x) {
  outfile <- tempfile()
  write.table(x, file=paste(outfile,"csv",sep="."), quote=F, sep=",", row.names=F, col.names=F, na="")
  system(paste("csvmidi", paste(outfile,"csv",sep="."), paste(outfile,"mid",sep=".")))
  system(paste("timidity",paste(outfile,"mid",sep=".")))
}

## Usage example
jj <- as.vector(sunspot.year, mode="numeric")[1:50]
jjn <- round(((jj-min(jj))*72/(max(jj)-min(jj))-36) + 70, 0)

ggmidi() + 
  audio_note(beat = 0:(length(jjn)-1), durs = 0.5, notes=jj, veloc = 127, program = 24) 


