import forcomp.Dictionary.getClass

import scala.annotation.tailrec
import scala.io.{Codec, Source}

List(Nil).size
List().size
Nil.size

List(('a', 2)) :: List(('b', 2))
List(('a', 2)) ++ List(('b', 2))
List(('a', 2)) :+ List(('b', 2))
List(List(('a', 2))) :+ List(('b', 2))
List(List(('a', 2))) ++ List(('b', 2))
List(List(('a',1), ('b',1)), List(('a',1), ('b',1), ('c',1)))
List(List(('a',1), ('b',1)), List(('a',1), ('b',1), ('c',1))) :: List(('d',1))
List(List(('a',1), ('b',1)), List(('a',1), ('b',1), ('c',1))) ++ List(('d',1))
List(List(('a',1), ('b',1)), List(('a',1), ('b',1), ('c',1))) :+ List(('d',1))

val elem = List(('a',2))
val acc = List(List(('b', 2)))
elem :: acc

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()

type Word = String

val word: Word = "Hello"

/** Ways to count the letters in a word */
//count method: 300 ms
val startTimeCount = System.currentTimeMillis()
val countWordCount = word.count(_ == 'l')
val finishTimeCount = System.currentTimeMillis()
val elapsedTimeCount = finishTimeCount - startTimeCount
println(s"Count Total time: $elapsedTimeCount")
//filter method: 64 ms
val startTimeFilter = System.currentTimeMillis()
val countWithFilter = word.filter(_ == 'l').size
val finishTimeFilter = System.currentTimeMillis()
val elapsedTimeFilter = finishTimeFilter - startTimeFilter
println(s"Filter Total time: $elapsedTimeFilter")
//groupBy method: Map[Char, Int] 64 ms
val startTimeGroupBy = System.currentTimeMillis()
val countWithGroupBy = word.groupBy(identity).view.mapValues(_.map(_ => 1).reduce(_ + _))('l')
val finishTimeGroupBy = System.currentTimeMillis()
val elapsedTimeGroupBy = finishTimeGroupBy - startTimeGroupBy
println(s"GroupBy Total time: $elapsedTimeFilter")
//recursive method 87 ms
val startTimeRecursive = System.currentTimeMillis()
val countWithRecursiveness: Int = {
  @tailrec
  def recursiveHelper(word: String, count: Int): Int =
    if (word.isEmpty) {
      count
    } else {
      val newCount = if (word.head == 'l') count + 1 else count
      recursiveHelper(word.tail, newCount)
    }

  recursiveHelper(word, 0)
}
val finishTimeRecursive = System.currentTimeMillis()
val elapsedTimeRecursive = finishTimeRecursive - startTimeRecursive
println(s"Recursive Total time: $elapsedTimeFilter")


/** Computing Anagrams of a word */
type Occurrences = List[(Char, Int)]

def wordOccurrences(w: Word): Occurrences =
  w.filter(_.isLetter).toLowerCase.groupBy(identity).view.mapValues(_.length).toSeq.sortBy(_._1).toList

//val dictionary: List[String] = List("hello", "olleh", "lolhe", "machine", "house", "uoshe")
//val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
//  dictionary.groupBy(wordOccurrences)


/** Computing Subsets of a Set */
val listOccurrences: List[(Char, Int)] = List(('a', 2), ('b', 2), ('c', 2))

val listOccurrencesElements =
  for (char, times) <- listOccurrences
  yield (char,times)

val listOccurrencesTimes = listOccurrences match
  case Nil => List()
  case x :: xs =>
    for
      (character, charTimes) <- (listOccurrences)
      times <- (0 to charTimes)
    yield (character, times)

def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match
  case Nil => List(Nil) // Empty set is a valid combination
  case (char, count) :: tail =>
    val tailCombinations = combinations(tail)
    // Combine current character with each combination from the tail
    tailCombinations ++ (for {
      i <- 1 to count
      rest <- tailCombinations
    } yield (char, i) :: rest)

val result = combinations(listOccurrences)
for(rest <- result) println(rest)


/** Subtract */
val listOccurrencesX = List(('a', 2),('b', 1),('c', 3))
listOccurrencesX.head
val mapOccurrencesX = listOccurrencesX.toMap
mapOccurrencesX.head._1
mapOccurrencesX.contains(mapOccurrencesX.head._1)
println(for (elem <- mapOccurrencesX) yield elem)
val mapOccurrenceXUpdated = mapOccurrencesX.updated('c', 1)
println(for (elem <- mapOccurrenceXUpdated) yield elem)
val listOccurrencesY = List(('c', 1))

def subtract (x: Occurrences, y: Occurrences): Occurrences = x match
  case Nil => List()
  case xs :: xs1 =>
    val resultMap = y.toMap.foldLeft(x.toMap)((xAcc, yMapElem) =>
      if (xAcc.contains(yMapElem._1) && xAcc.apply(yMapElem._1) >= yMapElem._2) xAcc.updated(yMapElem._1, xAcc(yMapElem._1) - yMapElem._2)
      else xAcc)
    resultMap.toList

subtract(listOccurrencesX, listOccurrencesY)


/** Return the anagrams of a sentence */
//type Sentence = List[Word]
//
//def sentenceOccurrences(s: Sentence): Occurrences =
//  wordOccurrences(s.mkString(""))
//
//val dictionary: List[Word] = Dictionary.loadDictionary
//lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
//  dictionary.groupBy(wordOccurrences)
//def wordAnagrams(word: Word): List[Word] =
//  dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())
//
//val sentence = "ab c"
//def sentenceAnagrams(sentence: Sentence): List[Sentence] =
//  val sentenceOccurrence: Occurrences = sentenceOccurrences(sentence)
//  val combinationsSentence: List[Occurrences] = combinations(sentenceOccurrence)
//  for(combination <- combinationsSentence){
//    val mapCombinationAnagram
//
//  }

//def sentenceSubtractCombination (combination: Occurrences, sentenceOccurrence: Occurrences, acc: List[Occurrences]): List[Occurrences] =
//  sentenceOccurrence match
//    case Nil => acc
//    case x :: xs => {
//      val newSentence: Occurrences = subtract(sentenceOccurrence, combination)
//      val combinationsNewSentence: List[Occurrences] = combinations(newSentence)
//      val newAcc =
//        for {
//          elem <- combinationsNewSentence
//          sentenceSubtractCombination(elem, newSentence, newAcc)
//          val newAcc: List[Occurrences] = elem :: acc
//
//        }
//    }
//


def sumValues (listOccurrences: Occurrences): Int =
  listOccurrences.foldLeft(0)((acc, pair) => acc + pair._2)



"Hello" + "hola"
def sentenceToWord(s: List[String]): String = s match
  case Nil => ""
  case x :: xs => x + sentenceToWord(xs)

val listExample = List("hello", "hola")
sentenceToWord(listExample)


val groupByResult = word.groupBy(identity).view.mapValues(_.length).toList
groupByResult.foreach(x => println(x))

val wordGrouped = word.toLowerCase.groupBy(x => word.count(_ == x))

val countWithCount = word.toLowerCase.count(_ == 'l')
val countWithFilter = word.toLowerCase.filter(_ == 'l').size

val groupByResult = word.groupBy(identity).view.mapValues(_.count(_ == 'l'))
val MapExample = Map('e' -> "e", 'l' -> "ll", 'H' -> "H", 'o' -> "o")
//val mapValues = MapExample.view.mapValues(_.map(_ => 1)).reduce(_+_)
//mapValues.foreach(x => println(x))


