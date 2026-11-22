# svg

A pure-V SVG module: parse, generate, inspect, flatten and rasterize SVG
documents without external dependencies.

The module is split into independent layers, so you can use only what you
need:

1. **Value types** — `Color`, `Length`, `Angle`, `Transform`, `ViewBox`,
   `AspectRatio`, `Path`
2. **Document tree** — `Element`, `Style`, CSS cascade, XML parse/write
3. **Fluent builder** — `svg.new(...)` for quick generation
4. **Resolution** — CSS/inheritance, transforms, `use`, `defs`, viewBox
5. **Flattening** — geometry to absolute paths with resolved paint
6. **Rasterization** — anti-aliased scanline fill/stroke, gradients, patterns,
   clips, filters, text

Only the V standard library is used: `encoding.xml`, `image`, `math`,
`strconv`, `strings` and `os`. There is no C interop and no runtime
dependency.

## Generate an SVG

```v
import svg
import os

fn main() {
	mut doc := svg.new(400, 200)
	doc.circle(100, 100, 60,
		svg.fill('#e74c3c'),
		svg.stroke('black'),
		svg.stroke_width(2))
	doc.rect(180, 60, 160, 80, svg.fill('#285aa0'))
	doc.text(20, 30, 'Hello SVG', svg.font_size(24), svg.fill('#333'))
	os.write_file('out.svg', doc.to_svg(svg.WriteOptions{})) or { panic(err.msg()) }
}
```

## Parse and inspect

```v
import svg

fn main() {
	doc := svg.Document.from_string('<svg width="10" height="10"><rect id="r"/></svg>') or {
		panic(err.msg())
	}
	rect := doc.root.find('r') or { panic('missing rect') }
	println(rect.tag)
}
```

## Rasterize to PNG

```v
import svg
import image.png
import os

fn main() {
	mut doc := svg.new(100, 100)
	doc.circle(50, 50, 40, svg.fill('#3498db'))
	img := doc.rasterize(svg.RasterOptions{
		width:   200
		height:  200
		samples: 4
	}) or { panic(err.msg()) }
	data := png.encode_to_bytes(img) or { panic(err.msg()) }
	os.write_file('icon.png', data.bytestr()) or { panic(err.msg()) }
}
```

## Working with paths directly

```v ignore
p := svg.parse_path('M10-20A5.5.3-4 110-.1')!
cubics := p.to_cubics()
points := p.flatten(0.1)
length := p.length()
scaled := p.transform(svg.Transform.scale(2, 2))
```

The path parser accepts compact SVG number syntax, implicit repeated
commands, exponent notation and single-digit arc flags. It converts implicit
commands to explicit ones and never panics on malformed input.

## Animation

SMIL elements (`animate`, `animateTransform`, `animateMotion`, `set`) and CSS
`@keyframes` are parsed into typed values. `snapshot_at(t)` evaluates them at
a given time and returns a static document:

```v ignore
frame := svg.snapshot_at(&doc, 1.5)!
img := frame.rasterize(svg.RasterOptions{ width: 200, height: 200 })!
```

There is no event loop or JavaScript execution. Scripting elements and `on*`
attributes are preserved verbatim but never run.

## Capabilities

Rendering fidelity is reported rather than silently approximated.

| Area | Status |
| --- | --- |
| Shapes, paths, transforms, viewBox | Complete |
| CSS subset (type/class/id selectors, cascade) | Common cases supported |
| `defs`, `use`, gradients, patterns, markers | Parsed and resolved |
| Fill rules, strokes, dashes, opacity, clips, masks | Supported |
| Linear/radial gradients with spread methods | Supported |
| SMIL + CSS keyframe representation | Supported via `snapshot_at` |
| Filters | Graph parsed in full; several primitives rasterized |
| Text | Built-in 3x5 ASCII bitmap renderer, or custom `TextDrawFn` |
| Scripting | Preserved as data, never executed |

Use `svg.supported_filters()` and `svg.filter_is_supported(f)` to query which
filter primitives can be rasterized. Unsupported primitives are reported
instead of producing silently wrong pixels.

## Design notes

The parsing and rendering stages are deliberately separate, following the
approach used by `usvg`/`resvg`. `flatten_document` produces a `Scene` that
can be inspected or rendered by a different backend. Geometry uses `f32`
coordinates and `f64` transforms; path commands are compact fixed-size
records. Elements keep their attributes in source order in a slice rather
than a map, which keeps output deterministic.

## License

Same as the V project (MIT).
