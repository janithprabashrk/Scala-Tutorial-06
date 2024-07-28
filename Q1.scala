object InventorySystem {

  type ProductID = Int
  type ProductDetails = (String, Int, Double)
  type Inventory = Map[ProductID, ProductDetails]

  val inv1: Inventory = Map(
    1 -> ("P1", 24, 15.50),
    2 -> ("P2", 5, 25.00),
    3 -> ("P3", 8, 12.75)
  )

  val inv2: Inventory = Map(
    2 -> ("P2", 3, 30.00),
    4 -> ("P4", 7, 20.00)
  )

  def getProductNames(inventory: Inventory): List[String] = {
    inventory.values.map(_._1).toList
  }

  def calculateTotalValue(inventory: Inventory): Double = {
    inventory.values.map { case (_, quantity, price) => quantity * price }.sum
  }

  def isInventoryEmpty(inventory: Inventory): Boolean = {
    inventory.isEmpty
  }

  def mergeInventories(inv1: Inventory, inv2: Inventory): Inventory = {
    inv2.foldLeft(inv1) {
      case (acc, (productId, (name, quantity, price))) =>
        acc.get(productId) match {
          case Some((existingName, existingQuantity, existingPrice)) =>
            acc + (productId -> (name, existingQuantity + quantity, math.max(existingPrice, price)))
          case None =>
            acc + (productId -> (name, quantity, price))
        }
    }
  }

  def checkProductDetails(inventory: Inventory, productId: ProductID): Unit = {
    inventory.get(productId) match {
      case Some((name, quantity, price)) =>
        println(s"Product ID: $productId, Name: $name, Quantity: $quantity, Price: $price")
      case None =>
        println("!!!....Product doesn't exist....!!!")
    }
  }

  def main(args: Array[String]): Unit = {
    println("Product Names in Inventory - 01 : " + getProductNames(inv1))
    println("Total Value of Inventory - 01 : " + calculateTotalValue(inv1))
    println("Is Inventory - 01 Empty ? : " + isInventoryEmpty(inv1))
    
    val mergedInv = mergeInventories(inv1, inv2)
    println("Merged Inventory : " + mergedInv)
    
    checkProductDetails(inv1, 2)
  }
}
