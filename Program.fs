#light

//
// Shyam Patel (NetID: spate54)
// U. of Illinois, Chicago
// CS 341, Fall 2018
// Project #05: Language prediction based on letter frequencies
//
// This program analyzes text from various languages, and computes
// letter frequencies.  These letter frequencies serve as a "barcode"
// that potentially identify the language when written.  This approach,
// and assignment, is inspired by the students and professor of CS 141,
// Fall 2018, at the U. of Illinois, Chicago.  Kudos to Prof Reed.
//


//
// explode:
//
// Given a string s, explodes the string into a list of alphabet characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode s =
  List.filter (fun e -> e >= 'a' && e <= 'z') [for c in s -> c]


//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string.
//
// let implode L =
//   let sb = System.Text.StringBuilder()
//   List.iter (fun c -> ignore (sb.Append (c:char))) L
//   sb.ToString()


//
// FileInput:
//
// Inputs text from a file line by line, returning this as a list
// of strings.  Each line is converted to lower-case.
//
let FileInput filename =
  [for line in System.IO.File.ReadLines(filename) -> line.ToLower()]


//
// UserInput:
//
// This function reads from the keyboard, line by line, until
// # appears on a line by itself.  The lines are returned as
// a list of strings; each line is converted to lower-case.
//
// NOTE: if the first line of input is blank (i.e. the user
// presses ENTER), then input is read from the file 'input.txt'.
// Helpful when testing.
//
let rec private _UserInput input =
  let line = System.Console.ReadLine()
  match line with
  | "#" -> List.rev input
  |  _  -> _UserInput (line.ToLower()::input)

let UserInput() =
  let firstLine = System.Console.ReadLine()
  match firstLine with
  | "" -> FileInput @"./input.txt"
  | _  -> _UserInput [firstLine.ToLower()]


//
// TrainingInput:
//
// This function reads from the training files, line by line.
// The lines are returned as a list of sublists of strings; each
// line is converted to lower-case.
//
let rec private _TrainingInput L input =
  match L with
  | []   -> List.rev input
  | e::r -> _TrainingInput r ((FileInput e)::input)

let TrainingInput L =
  _TrainingInput L []


//
// getFreqs:
//
// This function takes a list of strings, explodes the list into a sorted
// list of alphabet characters, and counts the number of each letter.
// The frequency counts are returned as a list of ( letter, count ) tuples
// sorted by descending frequency counts.
//
let rec private _getFreqs count letters L R =
  match letters, L with
  | []    , []                                     // done :
    -> List.rev (List.sortBy (fun (l, c) -> c) R)  //   ret descending freqs
  | []    , _                                      // should never happen
    -> []
  | e1::r1, []                                     // done counting 'z' :
    -> _getFreqs 0 r1 L ((e1, count)::R)           //   append tuple
  | e1::_ , e2::r2 when e1 = e2                    // found letter :
    -> _getFreqs (count + 1) letters r2 R          //   increment count
  | e1::r1, e2::_                                  // done counting letter :
    -> _getFreqs 0 r1 L ((e1, count)::R)           //   append tuple

let rec private _getCharacters L R =
  match L with
  | []   -> List.sort (List.concat R)              // ret sorted characters
  | e::r -> _getCharacters r (explode e::R)        // recursively explode

let getFreqs L =
  let characters = _getCharacters L []             // get sorted characters
  _getFreqs 0 ['a' .. 'z'] characters []           // call recursive helper


//
// TrainingFreqs:
//
// This function takes a list of sublists of strings (each sublist belongs
// to a language), explodes the sublists into sorted sublists of alphabet
// characters, and counts the number of each letter. The frequency counts
// are returned as a sublist of ( letter, count ) tuples sorted by
// descending frequency counts. Finally, the sublists are returned as a
// list of ( language, sublist of frequency counts ) tuples.
//
let rec private _TrainingFreqs2 lang freqs L =
  match L with
  | []   -> (lang, freqs)
  | e::r -> _TrainingFreqs2 e (getFreqs r) []

let rec private _TrainingFreqs1 L R =
  match L with
  | []   -> List.rev R
  | e::r -> _TrainingFreqs1 r ((_TrainingFreqs2 "" [] e)::R)

let TrainingFreqs L =
  _TrainingFreqs1 L []


//
// CountDiffs:
//
// This function computes the difference between the frequency counts of
// the user's input to that of every language in the training set. The
// difference is computed by analyzing the letters in order of highest to
// lowest frequency, and for each letter, computing the absolute value of
// the difference in index positions. The sum of differences that exceed
// the user-defined threshold are returned as a list of
// ( language, sum of differences ) tuples.
//
let rec private _CountDiffs2 thold lang sum letters L1 L2 =
  match letters with
  | []   -> (lang, sum)
  | e::r -> let pos1 = List.findIndex (fun x -> e = x) L1
            let pos2 = List.findIndex (fun x -> e = x) L2
            let diff = abs (pos1 - pos2)
            if diff > thold then
              _CountDiffs2 thold lang (diff + sum) r L1 L2
            else
              _CountDiffs2 thold lang sum r L1 L2

let private _CountDiffs1 thold lang L1 L2 =
  let letters1 = List.map (fun (letter, count) -> letter) L1
  let letters2 = List.map (fun (letter, count) -> letter) L2
  _CountDiffs2 thold lang 0 ['a' .. 'z'] letters1 letters2

let CountDiffs thold L1 L2 =
  let R = List.map (fun (lang, freqs) -> _CountDiffs1 thold lang freqs L2) L1
  List.sortBy (fun (lang, diffs) -> diffs) R


//
// printFreqCounts:
// printFreqOrder:
// printLangFreqCounts:
// printLangFreqOrder:
//
// These functions print letter frequency counts and letter frequency order
// for language training files and user input.
//
let printFreqCounts name L =
  printf  "%A: " name
  List.iter (fun (letter, count) -> printf "%A " count) (List.sort L)
  printfn ""

let printFreqOrder  name L =
  printf  "%A: " name
  List.iter (fun (letter, count) -> printf "%c" letter) L
  printfn ""

let printLangFreqCounts L =
  List.iter (fun (lang, freqs) -> (printFreqCounts lang freqs)) L

let printLangFreqOrder  L =
  List.iter (fun (lang, freqs) -> (printFreqOrder  lang freqs)) L


// *********************************************************************** //
//
// Main:
//
[<EntryPoint>]
let main argv =
  printfn "** Training... **"
  printfn ""
  //
  let langFiles = List.sort [for filename in System.IO.Directory.GetFiles(@"./training") -> filename]
  let langTexts = TrainingInput langFiles
  let langFreqs = TrainingFreqs langTexts
  //
  printfn "** Letter Frequency Counts (A->Z) **"
  printLangFreqCounts langFreqs
  printfn ""
  //
  printfn "** Letter Frequency Order (High->Low) **"
  printLangFreqOrder  langFreqs
  printfn ""
  //
  // Here we get text from the user, analyze, and guess the language:
  //
  printfn "Please enter text, followed by # (default = 'input.txt')> "
  let text  = UserInput()
  let freqs = getFreqs text
  printfn ""
  printFreqCounts "input" freqs
  printFreqOrder  "input" freqs
  printfn ""
  //
  printf "Enter difference threshold (default = 4)> "
  let s = System.Console.ReadLine()
  let threshold   = if s = "" then 4 else int(s)
  let differences = CountDiffs threshold langFreqs freqs
  printfn ""
  printfn "diffs: %A" differences
  printfn ""
  //
  let (prediction, _) = List.item 0 differences
  printfn "** Input language: %A" prediction
  printfn ""
  //
  0
