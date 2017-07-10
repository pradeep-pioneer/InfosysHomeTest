import scala.collection.mutable.ListBuffer
/**
  * Created by pradeep on 7/11/2017.
  *
  */
object Main {
  def findMissing(list:List[InvoiceData]) = {
    var missingInvoices:ListBuffer[(Int, List[Int])] = ListBuffer()
    var first = list.head
    for(next<-list.tail){
      if(first.endInvoiceNumber+1!=next.startInvoiceNumber&&first.category==next.category){
        val tuple = (first.category, (first.endInvoiceNumber+1 until next.startInvoiceNumber).toList)
        missingInvoices+=tuple
      }
      first = next
    }
    missingInvoices.map(x=>x._1).distinct.map(c=>(c, missingInvoices.filter(i=>i._1==c).flatMap(z=>z._2)))
  }

  def main(args:Array[String]):Unit={
    val invoiceList = List(InvoiceData(1,"A", 101, 104),InvoiceData(1,"B", 106, 107), InvoiceData(2,"A", 201, 205), InvoiceData(2,"B", 208, 210),InvoiceData(1,"C", 109, 113))
    val sorted = invoiceList.sortBy(x=>(x.category, x.startInvoiceNumber))
    println(sorted)
    val missing = findMissing(sorted)
    for(item<-missing){
      println(s"Category: ${item._1}\t\tInvoices: ${item._2.mkString(",")}")
    }
  }
}
