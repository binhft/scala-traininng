package exercises

import java.text.SimpleDateFormat
import java.time.{ LocalDate, ZoneId }
import java.util.Date

object ScalaDay3 extends App {
  /**
   * @author BinhPT
   *
   * Scala Training Practice exercises
   *
   * Updated: Solutions included. Please check and read the exercise's requirement below (at Exercises area)
   */
  // UTILS + CONSTANTS
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  val EMPTY = ""
  val NOT_AVAILABLE = "N/A"
  val DEFAULT_DATE_FORMAT = "yyyy/MM/dd"

  val DATE_FORMATTER = new SimpleDateFormat(DEFAULT_DATE_FORMAT)

  def toStr(v: Any): String = v match {
    case Some(x) => x.toString
    case None => EMPTY
    case v => v.toString
  }

  /**
   * Because the default implicit OptionOrdering ASC will return None first
   * --> Need customize the right position of pair parameters for compare() of Ordering class
   *
   * @param t1 same 1st param of your compare function
   * @param t2 same 2nd param of your compare function
   * @tparam T any Type
   */
  def ascNullLast[T](t1: Option[T], t2: Option[T]) = (t1, t2) match {
    case (Some(_), Some(_)) => (t1, t2)
    case _ => (t2, t1)
  }

  /**
   * Find number of different days from d2 to d1
   *
   * @param d1
   * @param d2
   * @return number of different days
   */
  def daysBetween(d1: LocalDate, d2: LocalDate): Int = (d1.toEpochDay - d2.toEpochDay).toInt

  /**
   * Common Implicit function to cast return value between different types
   */
  // Seq[Any] -> Seq[String
  implicit def sqAny2sqString(sq: Seq[Any]): Seq[String] = sq.map(toStr)

  def toSeqString(s: Seq[Any]): Seq[String] = s

  // String -> Date
  implicit def string2Date(d: String): Date = DATE_FORMATTER.parse(d)

  def str2D(d: String): Date = d

  // Date -> String
  implicit def date2String(d: Date): String = DATE_FORMATTER.format(d)

  // Date -> LocalDate
  implicit def date2LocalDate(d: Date): LocalDate =
    d.toInstant.atZone(ZoneId.systemDefault()).toLocalDate

  // DECLARE MODEL
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  case class Person(
    id: Long,
    name: Option[String],
    age: Option[Int],
    dLicense: Option[String], // Driving License
    vehicle: Option[Vehicle] = None // just add a tmp value, will update for calculation if need
  ) {
    /**
     * Clone to another object that update a calculated field `vehicle`
     */
    def clone()(implicit allVehicles: Seq[Vehicle]): Person =
      this.copy(vehicle = getVehicle(allVehicles))

    /**
     * Get vehicle for current person
     */
    def getVehicle(vehicles: Seq[Vehicle]): Option[Vehicle] =
      vehicles.find(_.ownerId.contains(id))

    /**
     * Check this person that is owning a vehicle and this vehicle is Full Info.
     */
    def isOwningFullInfoVehicle()(implicit allVehicles: Seq[Vehicle]): Boolean =
      getVehicle(allVehicles).exists(_.isFullInfo)
  }

  // Companion Object
  object Person {

    /**
     * Person ordering as Ex 3.6 -> Sort as below:
     *
     * > Number of parking DESC: Int
     * > Having License:         Option
     * > Name ASC:               Int
     * > Age ASC:                String
     * > Id ASC:                 Int
     *
     * Notes: For this above solution, only sorted after cloning to update `vehicle` attribute
     */
    implicit object PersonOrdering extends Ordering[Person] {
      def compare(x: Person, y: Person): Int = {
        val ordName = ascNullLast(x.name, y.name)
        val ordAge = ascNullLast(x.age, y.age)
        Ordering[(Option[Int], Boolean, Option[String], Option[Int], Long)].compare(
          (y.vehicle.map(_.parkingDates.length), x.dLicense.nonEmpty, ordName._1, ordAge._1, x.id),
          (x.vehicle.map(_.parkingDates.length), y.dLicense.nonEmpty, ordName._2, ordAge._2, y.id)
        )
      }
    }
  }

  case class Vehicle(
    id: Long,
    model: Option[String],
    color: Option[String],
    ownerId: Option[Long] // person Id who own that vehicle
  )(implicit
    allLogs: Seq[DailyParkingLog] = Seq.empty
  ) {
    // this check this object has full info
    val isFullInfo = toStr(model).nonEmpty && ownerId.nonEmpty && toStr(color).nonEmpty

    // this check "khách vãng lai"
    val isPassersBy = ownerId.isEmpty
    /**
     * For specific case, if you input (implicit) related information of this Vehicle
     * (allPerson != Seq.empty and allLogs != Seq.empty)
     *
     * --> You can use these Below values which were computed at time object initialized
     */
    //        // with each vehicle, you can find out one owner who registered this vehicle
    //        val owner             = allPerson.find(v => ownerId.contains(v.id))

    // with each vehicle, you can find out its history of parking date
    val parkingDates = getParkingDates(allLogs)
    val latestParkingDate = parkingDates.headOption

    /**
     * Filter a list parking logs of this Vehicle.
     *
     * @param logs : list data of parking logs to check
     * @return : all dates sorted by newest that the current vehicle has parking log
     */
    def getParkingDates(logs: Seq[DailyParkingLog]) =
      logs.filter(_.vIds.contains(id)).map(v => str2D(v.date)).sorted(Ordering[Date].reverse)

    def clone()(implicit logs: Seq[DailyParkingLog]): Vehicle =
      this.copy()(logs)
  }

  object Vehicle {

    /**
     * Vehicle ordering as Ex 3.5 -> Sort as below:
     *
     * > latestParkingDate: Date
     * > Model (Name) Asc:  String
     * > Id Asc:            Int
     */
    implicit object VehicleOrdering extends Ordering[Vehicle] {
      def compare(x: Vehicle, y: Vehicle): Int = {
        val ordModel = ascNullLast(x.model, y.model)
        Ordering[(Option[Date], Option[String], Long)].compare(
          (y.latestParkingDate, ordModel._1, x.id),
          (x.latestParkingDate, ordModel._2, y.id)
        )
      }
    }

  }

  case class DailyParkingLog(
    date: String, // Date but stored in String
    vIds: Seq[Long] // list of vehicle parked at that date
  ) {
    /**
     * To count number of passersby and not passerBy in this day
     *
     * @param vehicleMap map of all Vehicle
     * @return pair of two result passersby counted
     */
    def countPasser(vehicleMap: Map[Long, Vehicle]): (Int, Int) =
      vIds.flatMap(vehicleMap.get).foldLeft((0, 0))((r, v) => {
        if (v.isPassersBy) (r._1 + 1, r._2) // increase count passersBy
        else (r._1, r._2 + 1) // increase count Not passersBy
      }
      )
  }

  // MOCK DATA
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  implicit val DB_MOCK_PERSON: Seq[Person] = Seq(
    Person(1L, Some("Binh"), None, Some("LICENSE-1-B")),
    Person(2L, Some("Long"), Some(21), Some("LICENSE-2-L")),
    Person(3L, Some("Thanh"), Some(21), None),
    Person(4L, Some("Hieu"), Some(30), Some("LICENSE-4-H")),
    Person(5L, Some("Thuy"), None, Some("LICENSE-5-T")),
    Person(6L, Some("Hoa"), Some(18), Some("LICENSE-6-H")),
    Person(7L, None, None, None),
    Person(8L, None, Some(21), None)
  )

  implicit val DB_MOCK_PARKING_LOGS: Seq[DailyParkingLog] = Seq(
    DailyParkingLog("2021/07/20", Seq(1L, 2L, 3L, 4L, 5L, 6L, 7L)),
    DailyParkingLog("2021/07/21", Seq(1L, 2L, 3L, 4L, 5L, 6L, 7L, 9L, 10L)),
    DailyParkingLog("2021/07/22", Seq(1L, 2L, 4L, 5L, 6L, 9L)),
    DailyParkingLog("2021/07/23", Seq(1L, 2L, 3L, 4L, 5L, 7L)),
    DailyParkingLog("2021/07/24", Seq(1L, 3L, 5L, 6L, 10L)),
  )

  implicit val DB_MOCK_VEHICLE: Seq[Vehicle] = Seq(
    Vehicle(1L, Some("MK"), Some("Red"), Some(2L)),
    Vehicle(2L, Some("M2"), Some("Black"), Some(3L)),
    Vehicle(3L, Some("MK"), Some("White"), Some(1L)),
    Vehicle(4L, Some("M4"), Some("Black"), Some(4L)),
    Vehicle(5L, Some("M5"), None, Some(5L)),
    Vehicle(6L, Some("M6"), None, None),
    Vehicle(7L, Some("M7"), None, Some(8L)),
    Vehicle(8L, None, None, Some(6L)),
    Vehicle(9L, Some("M9"), None, None),
    Vehicle(10L, None, None, None)
  )

  // EXERCISE FUNCTIONS (*Plz use above mock data to do below exercises)
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  /**
   * --[ Ex 3.0* ]--------------------------------------------------
   *
   * This exercise is optional
   *
   * It's good if you can write a common function to draw table data by printing results
   * For example:
   * -------------------------------------------------------------------------
   * |    ID           |    Model        |    Color        |    Owner        |
   * -------------------------------------------------------------------------
   * |                 |                 |                 |                 |
   * |                 |                 |                 |                 |
   * |                 |                 |                 |                 |
   * -------------------------------------------------------------------------
   * Function can be declared like that:
   * def printResults(columnName: Seq[String], data: Seq[Seq[String]]): Unit
   */
  def printResults(columnName: Seq[String], data: Seq[Seq[String]]): Unit = {
    if (data.nonEmpty) {
      val table = Seq(columnName) ++ data
      // Get column widths based on the maximum cell width in each column (+2 for a one character padding on each side)
      val colWidths = table.transpose.map(_.map(cell => if (cell == null) 0 else cell.length).max + 2)
      // Format each row
      val rows = table.map(_.zip(colWidths).map { case (item, size) => (" %-" + (size - 1) + "s").format(item) }
        .mkString("|", "|", "|")
      )
      // Formatted separator row, used to separate the header and draw table borders
      val separator = colWidths.map("-" * _).mkString("+", "+", "+")
      // Put the table together and return
      println((separator +: rows.head +: separator +: rows.tail :+ separator).mkString("\n"))
    }
  }
  // ===================

  /**
   * --[ Ex 3.1 ]--------------------------------------------------
   *
   * Filter all Vehicles which owned by any person
   *
   * Format: Seq(Seq(ID, Model, Color, OwnerName))
   *
   * P/s: Empty string is replaced for None values
   */
  def allVehicleOwnedBy(): Seq[Seq[String]] = for {
    v <- DB_MOCK_VEHICLE
    p <- DB_MOCK_PERSON
    if v.ownerId.contains(p.id) // to filter vehicles owned
    // result a row as a seq
    res: Seq[String] = implicitly[Seq[Any]](Seq(v.id, v.model, v.color, p.name))
  } yield res
  // ===================

  /**
   * --[ Ex 3.2 ]--------------------------------------------------
   *
   * Get all Person with Vehicle model information (if having).
   * In case of not having any vehicle, set = `N/A`
   *
   * Format: Seq(Seq(ID, Name, Age, License, Vehicle))
   *
   * P/s: Empty string is replaced for None values
   */
  def allPersonWithOwnedVehicle(): Seq[Seq[String]] = for {
    p <- DB_MOCK_PERSON // get all person
    vName = DB_MOCK_VEHICLE
      .find(_.ownerId.contains(p.id)) // find related vehicle
      .map(_.model) // map to get vehicle name
      .getOrElse(NOT_AVAILABLE) // in case not found, set `N/A`
  } yield toSeqString(Seq(p.id, p.name, p.age, p.dLicense, vName))
  // ===================

  /**
   * --[ Ex 3.3 ]--------------------------------------------------
   *
   * Write a search function to return person list by Name keyword (search with `like` condition)
   *
   * Format: Seq(Seq(ID, Name, Age, License))
   *
   * For example:
   * with keyword = 'Bi' => result = [(1, Binh, , LICENSE-1-B)]
   *
   * P/s: Empty string is replaced for None values
   */
  def filterPersonByName(keyword: String): Seq[Seq[String]] = DB_MOCK_PERSON
    .filter(v => toStr(v.name).contains(keyword))
    .map(v => toSeqString(Seq(v.id, v.name, v.age, v.dLicense)))

  // ===================

  /**
   * --[ Ex 3.4 ]--------------------------------------------------
   *
   * Write a compare function for person following below priority condition:
   * Higher Age first, No Age last > Name follows alphabet DESC, no Name last > ID order ASC
   *
   * P/s: To test function => write unit test that use `sortWith` for Person list data
   */
  def comparePerson(p1: Person, p2: Person): Boolean = {
    def higher(c: Int): Boolean = if (c > 0) true else false

    p1.age.getOrElse(0) - p2.age.getOrElse(0) match { // Compare Age:
      case 0 => toStr(p1.name).compareTo(toStr(p2.name)) match { // Equal Age => Compare Name:
        case 0 => p1.id < p2.id // Equal Name => Compare Id ASC
        case c => higher(c) // Differ Name => return alphabet Desc
      }
      case c => higher(c) // Differ Age => return higher Age.
    }
  }

  // ===================

  /**
   * --[ Ex 3.5 ]--------------------------------------------------
   *
   * Thực hành sort list toàn bộ Vehicle theo điều kiện dưới:
   *
   * Vehicle có thời gian gửi xe cuối cùng mới nhất
   * > Model alphabet ASC, None last
   * > ID ASc
   *
   * Notes: Có thể thoải mái sáng tạo thêm các common functions, hoặc hàm support ở các class model
   *
   */
  def sortVehicle()(implicit vehicles: Seq[Vehicle]): Seq[Vehicle] = vehicles.sorted

  // ===================

  /**
   * --[ Ex 3.6 ]--------------------------------------------------
   *
   * Thực hành sort list toàn bộ Person theo điều kiện dưới:
   *
   * + Người có thống kê số ngày đỗ xe nhiều nhất
   * > Có Driving License, None last
   * > Name alphabet ASC, None last
   * > Age ASC, None last
   * > ID order ASC
   *
   * Yêu cầu: Có thể thiết kế thêm các hàm common hoặc giá trị constant hoặc hàm compare cho class Person
   * -> Làm sao mục đích gọi: DB_MOCK_PERSON.sorted => trả về list data đc sort theo yêu cầu
   *
   */
  def sortPerson()(implicit persons: Seq[Person]): Seq[Person] =
    persons.map(_.clone).sorted

  // ===================

  /**
   * --[ Ex 3.7 ]--------------------------------------------------
   *
   * Tìm ngày có số người gửi vãng lai nhiều nhất, và ngày có số người gửi xác thực ít nhất
   * Nếu có hơn 1 kết quả, lấy ngày gần mới nhất
   *
   * ** Khách vãng lai đc xác định là Xe gửi mà không có thông tin Owner,
   * Xe có thông tin owner là xe có đăng ký người gửi xác thực
   *
   */
  def datePasser()(implicit logs: Seq[DailyParkingLog], vehicles: Seq[Vehicle]): (String, String) = {
    // trường hợp dữ liệu lớn, tạo 1 map rồi get sẽ tối ưu hơn truyền list rồi dùng `find`
    val allVehicleMap = vehicles.map(v => v.id -> v).toMap

    def extreme(v1: (String, Int), v2: (String, Int), compareMax: Boolean) =
      v1._2 - v2._2 match {
        // in case of same value, choose pair has nearest date
        case 0 => if (str2D(v1._1).compareTo(str2D(v2._1)) > 0) v1 else v2 // choose nearest date
        case x if x > 0 => if (compareMax) v1 else v2
        case _ => if (compareMax) v2 else v1
      }

    val res = logs.map(v => {
      val countPasser = v.countPasser(allVehicleMap)
      // set counted value to key is the current date
      (v.date -> countPasser._1, v.date -> countPasser._2)
    }
    ).reduce((x, y) =>
      (extreme(x._1, y._1, compareMax = true), // compare get max pair passersBy
        extreme(x._2, y._2, compareMax = false))
    ) // compare get min pair not PassersBy
    // return value: is the date
    (res._1._1, res._2._1)
  }

  // ===================

  /**
   * --[ Ex 3.8 ]--------------------------------------------------
   *
   * Phân chia Person làm 3 loại:
   *
   * - Verified: Thỏa mãn 1 trong các điều kiện sau:
   * + Có full info: name, age, license
   * + Có tối thiểu License + owning 1 phương tiện nào đó có full info (model, color)
   * hoặc ít nhất có thông tin Model thì phải có dữ liệu gửi xe trong 10 ngày gần nhất
   * - Normal: 1 trong đk:
   * + Có license + name
   * + Có ít nhất name + owning 1 phương tiện nào đó có full info (model, color)
   * hoặc ít nhất có thông tin Model thì phải có dữ liệu gửi xe trong 10 ngày gần nhất
   * - Lack info: Còn lại
   *
   * ** Thống kê từ data từng loại trên có số lượng là bao nhiêu
   */
  def countPersonType()(implicit
    allPersons: Seq[Person],
    allVehicles: Seq[Vehicle],
    allLogs: Seq[DailyParkingLog]
  ): Map[String, Int] = {
    // ---------Prepare data------------
    val currentDate = new Date()
    // that we only count data for this input logs
    val logs10Nearest = allLogs.filter(v => daysBetween(currentDate, str2D(v.date)) < 10)
    // find all vehicles has data at 10 nearest
    val vehicles10Nearest = allVehicles.map(_.clone()(logs10Nearest)).filter(_.latestParkingDate.nonEmpty)
    // assign persons to new one that person has data logs in 10 days will be assigned `vehicle` attribute
    val persons = allPersons.map(_.clone()(vehicles10Nearest))

    /**
     * It's validated if match one of below conditions:
     *
     * + Owning a full info vehicle
     *
     * + Or: Vehicle has model name and has parking data logs at nearest 10 days (included today)
     *
     * @param person who to check
     * @return
     */
    def validatedOwning(person: Person): Boolean =
      person.isOwningFullInfoVehicle()(allVehicles) || // Owning a full info vehicle
        (person.vehicle.nonEmpty && person.vehicle.get.model.nonEmpty) // Vehicle has model name and

    // has parking data logs at nearest 10 days
    // ---------Calculate------------
    // Khởi tạo 3 giá trị cần tính = 0 rồi fold data
    val foldData = persons.foldLeft((0, 0, 0))((f, p) => (p.dLicense, p.name, p.age) match {
      case (Some(_), Some(_), Some(_)) => (f._1 + 1, f._2, f._3) // full data => + Verified
      case (Some(_), _, _) if validatedOwning(p) => (f._1 + 1, f._2, f._3) // at least License + validated => + Verified
      case (Some(_), Some(_), _) => (f._1, f._2 + 1, f._3) // name + license => + Normal
      case (_, Some(_), _) if validatedOwning(p) => (f._1, f._2 + 1, f._3) // at least Name + validated => + Normal
      case (_, _, _) => (f._1, f._2, f._3 + 1) // Otherwise => + Lack Info
    }
    )
    // ---------Return Result------------
    Map(
      "Verified" -> foldData._1,
      "Normal" -> foldData._2,
      "Lack" -> foldData._3
    )
  }

  // UNIT TEST
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  //    val res = DB_MOCK_PERSON.sortWith(comparePerson)
  //    printResults(
  //        Seq("ID", "Name", "Color"),
  //        DB_MOCK_VEHICLE.sorted.map(v => toSeqString(Seq(v.id, v.model, v.color)))
  //    )
  //    println(DB_MOCK_PARKING_LOGS.map(_.countPasser(DB_MOCK_VEHICLE.map(v => v.id -> v).toMap)))
  println(countPersonType)
}
