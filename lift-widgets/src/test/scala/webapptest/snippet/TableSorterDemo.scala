package webapptest.snippet

import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.widgets.tablesorter.TableSorter

class TableSorterDemo {
  def render(xhtml: NodeSeq) :NodeSeq = {
    val widget = new TableSorter()
    widget.init()
    <xml:group>
    {widget.render("table")}
<table id="myTable" class="tablesorter">
<thead>
<tr>
    <th>Last Name</th>
    <th>First Name</th>
    <th>Email</th>
    <th>Due</th>
    <th>Web Site</th>
</tr>
</thead>
<tbody>
<tr>
    <td>Smith</td>
    <td>John</td>
    <td>jsmith@gmail.com</td>
    <td>$50.00</td>
    <td>http://www.jsmith.com</td>
</tr>
<tr>
    <td>Bach</td>
    <td>Frank</td>
    <td>fbach@yahoo.com</td>
    <td>$50.00</td>
    <td>http://www.frank.com</td>
</tr>
<tr>
    <td>Doe</td>
    <td>Jason</td>
    <td>jdoe@hotmail.com</td>
    <td>$100.00</td>
    <td>http://www.jdoe.com</td>
</tr>
<tr>
    <td>Conway</td>
    <td>Tim</td>
    <td>tconway@earthlink.net</td>
    <td>$50.00</td>
    <td>http://www.timconway.com</td>
</tr>
</tbody>
</table>
</xml:group>
  }
}