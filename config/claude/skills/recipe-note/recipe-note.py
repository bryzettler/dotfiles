#!/usr/bin/env python3
"""Create an Apple Note in the Recipes folder from a recipe web page.

Reads the page's schema.org Recipe data (JSON-LD) and writes it in the
house style of the existing Recipes notes: title, a servings/time line,
an ingredient block, and numbered steps.

Usage: recipe-note.py (URL | --recipe-json FILE) [--title TITLE] [--folder FOLDER] [--dry-run]
"""
import argparse
import html
import json
import re
import subprocess
import sys
import urllib.request

RULE = "—" * 21
LIGHT = "#CACACA"
MID = "#707070"

UNITS = {
    "pound": "lb", "pounds": "lb", "lb": "lb", "lbs": "lb",
    "ounce": "oz", "ounces": "oz", "oz": "oz",
    "tablespoon": "tbsp", "tablespoons": "tbsp", "tbsp": "tbsp",
    "teaspoon": "tsp", "teaspoons": "tsp", "tsp": "tsp",
    "cup": "cup", "cups": "cup",
    "gram": "g", "grams": "g", "g": "g",
    "kilogram": "kg", "kilograms": "kg", "kg": "kg",
    "ml": "ml", "milliliter": "ml", "milliliters": "ml",
    "liter": "l", "liters": "l",
    "clove": "", "cloves": "",
    "can": "can", "cans": "can",
}
FRACTIONS = {"1/2": "½", "1/3": "⅓", "2/3": "⅔", "1/4": "¼", "3/4": "¾", "1/8": "⅛"}
SMALL_WORDS = {"or", "and", "of", "in", "a", "the", "with", "for", "to"}


def fetch_recipe(url):
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0 (Macintosh) recipe-note"})
    page = urllib.request.urlopen(req, timeout=30).read().decode("utf-8", "replace")
    for block in re.findall(r'<script[^>]*application/ld\+json[^>]*>(.*?)</script>', page, re.S):
        try:
            data = json.loads(block)
        except json.JSONDecodeError:
            continue
        for item in data if isinstance(data, list) else data.get("@graph", [data]):
            if "Recipe" in str(item.get("@type")):
                size = page_serving_size(page)
                if size and not serving_size(item):
                    item.setdefault("nutrition", {})["servingSize"] = size
                return item
    sys.exit(f"No schema.org Recipe data found at {url}")


def page_serving_size(page):
    """Find a serving size printed on the page, e.g. 'Amount Per Serving (1 cup)'."""
    text = html.unescape(re.sub(r"\s+", " ", re.sub(r"<[^>]+>", " ", page)))
    m = (re.search(r"Per Serving \(([^)]{1,30})\)", text, re.I)
         or re.search(r"Serving Size:? ([\w./½⅓⅔¼¾ -]{1,30}?)(?= ?[|,;:(]| Calories| Servings|$)", text, re.I))
    return m.group(1).strip() if m else None


def serving_size(recipe):
    """The size of one serving, from the yield or the nutrition data.

    '6 servings (1⅓ cups each)' and '1 serving (1⅓ cups each, without add-ins)'
    both give '1⅓ cups'. A bare '1 serving' gives ''.
    """
    yields = recipe.get("recipeYield")
    nutrition = str((recipe.get("nutrition") or {}).get("servingSize") or "").strip()
    sizes = [m.group(1) for y in (yields if isinstance(yields, list) else [yields])
             for m in [re.search(r"\((.*)\)", str(y or ""))] if m]
    m = re.search(r"\((.*)\)", nutrition)
    if m:
        sizes.append(m.group(1))
    elif not re.fullmatch(r"\d+ ?(serving|portion)s?", nutrition, re.I):
        sizes.append(nutrition)
    for size in sizes:
        size = re.sub(r"\s+each\b", "", size.split(",")[0]).strip()
        if size:
            return size
    return ""


def fraction(amount):
    amount = re.sub(r"\b(\d+)/(\d+)\b", lambda m: FRACTIONS.get(m.group(0), m.group(0)), amount)
    return re.sub(r"(\d) ([½⅓⅔¼¾⅛])", r"\1\2", amount)


def title_case(text):
    words = text.split()
    return " ".join(w if (i and w.lower() in SMALL_WORDS) or not w[0].isalpha() else w[0].upper() + w[1:]
                    for i, w in enumerate(words))


def parse_ingredient(line):
    """Split '1 (15-ounce) can black beans (rinsed)' into amount, name and note."""
    line = html.unescape(re.sub(r"\s+", " ", line)).strip()
    # Drop cost notes ("$0.70", "$1.52*") and flatten "(chopped, (about 1 lb.))".
    line = re.sub(r",?\s*\$[\d.]+\**", "", line)
    while re.search(r"\([^()]*\(", line):
        line = re.sub(r"\(([^()]*)\(([^()]*)\)([^()]*)\)", r"(\1\2\3)", line)
    notes = [n for n in re.findall(r"\(([^)]*)\)", line) if n.strip()]
    m = re.match(r"([\d./½⅓⅔¼¾⅛\s-]+)?\s*(?:\((\d+(?:\.\d+)?)[- ]?(ounce|oz)\)\s*(cans?)\s+)?(?:([a-zA-Z]+\.?)(?=\s|$))?\s*(.*)", line)
    qty, can_size, _, can, unit, rest = m.groups()
    qty = (qty or "").strip()
    if can:
        # "2 (15-ounce) cans tomatoes" -> "30 oz Canned Tomatoes"
        count = sum(float(a) / float(b or 1) for a, _, b in re.findall(r"(\d+)(/(\d+))?", qty)) if qty else 1
        total = count * float(can_size)
        amount, name = f"{total:g} oz", "Canned " + (unit or "") + " " + rest
        notes = notes[1:]
    elif unit and unit.lower().rstrip(".") in UNITS:
        amount, name = f"{fraction(qty)} {UNITS[unit.lower().rstrip('.')]}".strip(), rest
    else:
        amount, name = fraction(qty), f"{unit or ''} {rest}"
    name = re.sub(r"\s*\([^)]*\)", "", name).strip(" ,")
    return amount, title_case(name), ", ".join(notes)


def duration(iso):
    """'PT1H20M' -> '1h20m'; 'PT15M' -> '15m'."""
    m = re.match(r"P(?:T)?(?:(\d+)H)?(?:(\d+)M)?", iso or "")
    if not m or not any(m.groups()):
        return "-"
    h, mins = (int(x or 0) for x in m.groups())
    return (f"{h}h" if h else "") + (f"{mins}m" if mins or not h else "")


def servings(recipe):
    y = recipe.get("recipeYield")
    y = y[0] if isinstance(y, list) else y
    return re.match(r"\d+", str(y)).group(0) if y and re.match(r"\d+", str(y)) else "-"


def steps(recipe):
    out = []
    for item in recipe.get("recipeInstructions") or []:
        if isinstance(item, str):
            out.append(item)
        elif item.get("@type") == "HowToSection":
            out.extend(s.get("text", "") for s in item.get("itemListElement", []))
        else:
            out.append(item.get("text", ""))
    return [html.unescape(re.sub(r"<[^>]+>", "", s)).strip() for s in out if s.strip()]


# Notes' HTML import drops font sizes unless the font family is named, and
# turns <pre> into the grey-backed Monospaced paragraph style.
def text(t, size=None, hex_=None, family="-apple-system"):
    css = [f"font-family: {family}"] + ([f"font-size: {size}px"] if size else []) + ([f"color: {hex_}"] if hex_ else [])
    return f'<span style="{"; ".join(css)}">{html.escape(t)}</span>'


def blank(size=14):
    return f'<div><span style="font-family: -apple-system; font-size: {size}px"><br></span></div>'


def build_html(title, recipe):
    title = html.unescape(title)
    size = serving_size(recipe)
    meta = [(servings(recipe), "servings" + (f" ({fraction(size)})" if size else "")), (duration(recipe.get("prepTime")), "prep"),
            (duration(recipe.get("cookTime")), "cook"), (duration(recipe.get("totalTime")), "total")]
    meta_html = " | ".join(f'<b>{text(v, family="ui-monospace")}</b>{text(" " + label, 11, family="ui-monospace")}'
                           for v, label in meta).replace("</span> | <b>", "</span>" + text(" | ", 11, family="ui-monospace") + "<b>")
    rows = [
        f"<div><b>{text(title, 21)}</b></div>",
        f"<pre>{meta_html}</pre>",
        blank(),
        f'<div><font color="{LIGHT}">{RULE}</font></div>',
        blank(),
    ]
    for line in recipe.get("recipeIngredient") or []:
        amount, name, note = parse_ingredient(line)
        row = (f"<b>{text(amount + ' ', 14)}</b>" if amount else "") + text(name + (" " if note else ""), 14, MID)
        if note:
            row += f"<i>{text(title_case(note), 14, LIGHT)}</i>"
        rows.append(f"<div>{row}</div>")
    rows += [blank(), f'<div><font color="{LIGHT}">{RULE}</font></div>', blank()]
    for i, step in enumerate(steps(recipe), 1):
        if i > 1:
            rows.append(blank(15))
        rows.append(f"<div>{text(f'{i} ', 15, LIGHT)}{text(step, 15, MID)}</div>")
    return "\n".join(rows)


CREATE_NOTE = '''
on run argv
    set noteBody to item 1 of argv
    set folderName to item 2 of argv
    tell application "Notes"
        make new note at folder folderName with properties {body:noteBody}
    end tell
end run
'''


def main():
    p = argparse.ArgumentParser(description="Create a Recipes note from a recipe URL.")
    p.add_argument("url", nargs="?")
    p.add_argument("--recipe-json", help="read a schema.org Recipe object from this file instead of fetching URL")
    p.add_argument("--title", help="note title (default: the recipe's own name)")
    p.add_argument("--folder", default="Recipes")
    p.add_argument("--dry-run", action="store_true", help="print the note HTML and do not create it")
    args = p.parse_args()

    if bool(args.url) == bool(args.recipe_json):
        p.error("give exactly one of URL or --recipe-json")
    if args.recipe_json:
        with open(args.recipe_json) as f:
            recipe = json.load(f)
    else:
        recipe = fetch_recipe(args.url)
    body = build_html(args.title or recipe.get("name", "Recipe"), recipe)
    if args.dry_run:
        print(body)
        return
    subprocess.run(["osascript", "-", body, args.folder], input=CREATE_NOTE, text=True, check=True)
    print(f"Created note '{html.unescape(args.title or recipe.get('name', 'Recipe'))}' in folder '{args.folder}'.")


if __name__ == "__main__":
    main()
