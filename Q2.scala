import scala.io.StdIn._

object StudentRecManager {

  def main(args: Array[String]): Unit = {
    val studentInfo = getStudentInfoWithRetry()
    printStudentRecord(studentInfo)
  }

  def getStudentInfo(): (String, Int, Int, Double, Char) = {
    val name = readLine("Enter student name: ")
    val marks = readLine("Enter student marks: ").toInt
    val totalMarks = readLine("Enter total possible marks: ").toInt

    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = if (percentage >= 90) 'A'
                else if (percentage >= 75) 'B'
                else if (percentage >= 50) 'C'
                else 'D'

    (name, marks, totalMarks, percentage, grade)
  }

  def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
    println(s"Student Name : ${student._1}")
    println(s"Marks : ${student._2}")
    println(s"Total Marks : ${student._3}")
    println(s"Percentage : ${student._4}")
    println(s"Grade : ${student._5}")
  }

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some("Marks must be positive and not exceed total marks"))
    } else {
      (true, None)
    }
  }

  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var valid = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'F')

    while (!valid) {
      val name = readLine("Enter student name: ")
      val marks = readLine("Enter student marks: ").toInt
      val totalMarks = readLine("Enter total possible marks: ").toInt

      val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
      if (isValid) {
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = if (percentage >= 90) 'A'
                    else if (percentage >= 75) 'B'
                    else if (percentage >= 50) 'C'
                    else 'D'
        studentInfo = (name, marks, totalMarks, percentage, grade)
        valid = true
      } else {
        println(s"Invalid input: ${errorMessage.get}")
      }
    }
    studentInfo
  }
}
