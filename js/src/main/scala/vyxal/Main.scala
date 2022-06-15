package vyxal

import scala.scalajs.js

import org.scalajs.dom
import org.scalajs.dom.{document, window}

object Main {
  def main(args: Array[String]): Unit = {

    window.addEventListener(
      "DOMContentLoaded",
      event => {
        document.body.appendChild(
          JSVyxal.htmlFrag
        )

        JSVyxal.runButton.onclick(null)
        JSVyxal.runButton.addEventListener("click", _ => JSVyxal.run())
        document.getElementById("run_button").asInstanceOf[dom.html.Button].onclick = JSVyxal.runButton.onclick

        // val kb = document.getElementById("keyboard")
        // for (i <- codepage.indices) {
        //   kb.innerHTML +=
        //     s"""<span class=\"key\" style="text-align:center;"
        //           title='${repr(codepage_descriptions[i])}'>${codepage[
        //       i
        //     ]}</span>""""
        // }
        // for (item <- document.querySelectorAll(".key")) {
        //   item.addEventListener(
        //     "click",
        //     event => {
        //       val char = replaceHTMLChar(event.target.innerHTML)
        //       JSVyxal.updateCount()
        //     }
        //   )
        // }
      }
    )

    document.addEventListener(
      "keydown",
      (event: dom.KeyboardEvent) => {
        if (event.metaKey && event.key == "Enter") {
          JSVyxal.runButton.click()
        }
      }
    )

  }
}
