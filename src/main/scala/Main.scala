import scala.collection.mutable.ListBuffer
/**
  * Created by pradeep on 7/11/2017.
  *
  */
object Main {

  def findMissing(list: List[Int]) = {
    def findMissingR(first:Int, remaining:List[Int], acc:List[Int]):List[Int] = remaining match{
      case Nil=> Nil
      case first::last::Nil=>{
        if(first+1!=last) (first+1 until last).toList:::acc
        else acc
      }
      case head::tail=>{
        if(first+1!=head){
          val missing = (first+1 until head).toList
          findMissingR(head, tail, missing:::acc)
        }
        else
          findMissingR(head, tail, acc)
      }
    }
    if(list.length<=0) List[Int]()
    else{
      findMissingR(list.head, list.tail, List[Int]())
    }
  }

  def main(args:Array[String]):Unit={
    val invoiceList = List(InvoiceData(1,"A", 101, 104),InvoiceData(1,"B", 106, 107), InvoiceData(2,"A", 201, 205), InvoiceData(2,"B", 208, 210),InvoiceData(1,"C", 109, 113), InvoiceData(1, "D", 105, 108))
    val categoryInvoices = invoiceList.map(x=>(x.category, x.productId, (x.startInvoiceNumber to x.endInvoiceNumber).toList))
     .flatMap(x=>List((x._1, x._3)))
    val categoryInvoicesFlat =  categoryInvoices.map(c=>c._1).distinct.map(x=>(x, categoryInvoices.filter(y=>y._1==x).map(z=>z._2).flatten.distinct.sorted))
    val missingInvoices = categoryInvoicesFlat.map(x=>(x._1, findMissing(x._2))).filter(x=>x._2.length>0)
    for(item<-missingInvoices)
      println(s"Category: ${item._1}\t\t Missing: ${item._2}")
  }
}
