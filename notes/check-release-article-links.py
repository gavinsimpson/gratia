"""Check local links, anchors and image files in the rendered release articles."""
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

root = Path("docs").resolve()
names = ["model-checking", "gfam-support", "glmmTMB-support", "sdmTMB-support",
         "prediction-comparisons", "smooth-effects", "spatial-smooths",
         "response-distributions", "index"]

class Page(HTMLParser):
    def __init__(self, text):
        super().__init__()
        self.targets = []
        self.ids = set()
        self.feed(text)

    def handle_starttag(self, tag, attributes):
        attributes = dict(attributes)
        if "id" in attributes:
            self.ids.add(attributes["id"])
        key = "href" if tag == "a" else "src" if tag == "img" else None
        if key and key in attributes:
            self.targets.append(attributes[key])

pages = {}
errors = []
checked = 0
for name in names:
    source = root / "articles" / (name + ".html")
    page = Page(source.read_text())
    for target in page.targets:
        parsed = urlsplit(target)
        if parsed.scheme or parsed.netloc:
            continue
        path = (source.parent / unquote(parsed.path)).resolve() if parsed.path else source
        if path.is_dir():
            path = path / "index.html"
        checked += 1
        if not path.exists():
            errors.append(f"{source.name}: missing {target}")
        elif parsed.fragment and path.suffix == ".html":
            if path not in pages:
                pages[path] = Page(path.read_text())
            if unquote(parsed.fragment) not in pages[path].ids:
                errors.append(f"{source.name}: missing anchor {target}")
if errors:
    raise SystemExit("\n".join(errors))
print(f"Checked {checked} local links, anchors and images across {len(names)} pages.")
