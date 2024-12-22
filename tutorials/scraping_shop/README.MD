# ScrapingShop 🕷️🛒

## Description

ScrapingShop is a web scraping application developed with the V web framework (veb), which captures product information from platforms such as Amazon and Instant Gaming.

## Main Features

- 🌐 Data scraping from multiple platforms
- 💾 Saving to SQLite database
- 🎨 Neubrutalism interface with light/dark themes
- 🌍 Multilingual support (PT-BR, EN and RU)
- 🔄 Real-time communication with Server-Sent Events (SSE)

## Technologies

- Web Framework: veb
- Frontend: HTML, CSS (pure), HTMX
- Database: SQLite

## Template Rendering

The application uses the native features of the veb language and framework, with two main methods:

- $tmpl(): For rendering components
- $veb.html(): For rendering complete pages

## Server-Sent Events (SSE)

The application implements SSE for real-time communication, especially in the language selection component:

**Implementation Example SSE**
```v
@['/current_lang']
fn (page &PageIndex) current_lang(mut ctx wcontext.WsCtx) veb.Result {
    ctx.takeover_conn()
    go fn [page] (mut ctx wcontext.WsCtx) {
        mut sse_conn := sse.start_connection(mut ctx.Context)
        for {
            rlock page.current_lang {
                sse_conn.send_message(
                    event: 'langName'
                    data:  page.current_lang.lang
                ) or { continue }
            }
            time.sleep(750 * time.millisecond)
        }
        sse_conn.close()
    }(mut &ctx)

    return veb.no_result()
}
```

**Client Side (HTML)**
```html
<div class="dropdown">
    <button style="height: 52px;" class="dropdown-button">
        <div hx-ext="sse" sse-connect="/home/current_lang" sse-swap="langName"></div>
    </button>
    <!-- Resto do código do dropdown -->
</div>
```

## Native Rendering Example with $tmpl

```v
pub fn construct(abbrev_lang string) veb.RawHtml {
    return $tmpl('./switch_lang.html')
}
```

## Componentization

The interface is built in a modular way, with independent components:
- Language switch
- Status badges
- Dark mode toggle
- Scraping result cards

## Multilingual support

Implemented through veb's integrated translation module:
Translation files (pt-br.tr and en.tr)
Dynamic translation with veb.tr()

## Supported Platforms

📚 Amazon (Books)
🎮 Instant Gaming (Games)

## Benefits

Component modularity
Native internationalization
Efficient rendering
Responsive and adaptive interface