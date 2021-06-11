import net.http

const url = "https://www.janusfiresystems.com/downloads/PDF/DS1002_Lv%20Series_FM-200_System.pdf"

fn main() {
  resp := http.get(url)?
  println(resp.status_code)
}
