---
name: recipe-note
description: Save a recipe from a web page as an Apple Note in the Recipes folder, in the house style of the existing recipe notes. Use when the user gives a recipe URL to add, save, or turn into a note.
---

# recipe-note

`recipe-note.py` (next to this file) reads a page's schema.org Recipe data and creates the note through AppleScript. Run it with the folder path: `python3 <skill-dir>/recipe-note.py`.

## Steps

1. Preview: `recipe-note.py URL --title "Title" --dry-run | sed -E 's/<[^>]+>//g'`. Use the user's title when they give one; otherwise omit `--title` and the page's recipe name is used.
2. Check every ingredient line against the page's recipe. The parser is a heuristic, so look for a merged amount, a lost unit, or a note glued onto the name. If the page has no recipe data, or a line comes out wrong, write a schema.org Recipe JSON file in the scratchpad (`name`, `recipeYield`, `prepTime`/`cookTime`/`totalTime` as ISO 8601 durations, `nutrition.servingSize` such as `1 cup`, `recipeIngredient`, `recipeInstructions`) and use `--recipe-json FILE` in place of URL.
   If the fetch fails with HTTP 403 (bot protection, for example budgetbytes.com), read the page through a real browser instead. See [Blocked sites](#blocked-sites).
3. Create: the same command without `--dry-run`.
4. Verify: `osascript -e 'tell application "Notes" to get body of note "Title" of folder "Recipes"'`. The servings line is in `<tt>`, the ingredients and steps carry `font-size: 14px` / `15px`. Done when those show.
5. Tell the user anything the recipe uses that its ingredient list leaves out (for example water named only in a step).

## Blocked sites

The chrome-devtools MCP (`mcp__chrome-devtools__*`, loaded through ToolSearch) runs its own Brave window with a temporary profile. The script and WebFetch get a 403 from some sites; this browser does not.

1. `new_page` with the recipe URL and `background: true`. Take the page id from the result's page list, not from an earlier call: the ids change whenever the browser restarts.
2. `evaluate_script` on that page id with `waitForStableDom: false` and this function. Do not pass `filePath`; the MCP cannot write to the scratchpad, so take the JSON from the reply.

   ```js
   () => {
     for (const s of document.querySelectorAll('script[type="application/ld+json"]')) {
       const d = JSON.parse(s.textContent);
       for (const it of Array.isArray(d) ? d : (d['@graph'] || [d]))
         if (String(it['@type']).includes('Recipe')) {
           const {name, recipeYield, prepTime, cookTime, totalTime, nutrition, recipeIngredient, recipeInstructions} = it;
           return {name, recipeYield, prepTime, cookTime, totalTime, nutrition: {servingSize: nutrition?.servingSize},
                   recipeIngredient, recipeInstructions: recipeInstructions.map(i => i.text ?? i)};
         }
     }
     return null;
   }
   ```

3. Write the reply to a JSON file in the scratchpad and continue at step 2 with `--recipe-json FILE`.
4. Close the browser when the JSON file is written: `pkill -f -- --remote-debugging-pipe`. `close_page` cannot close the last tab, so quit the process. Only browsers that the MCP launched carry that flag, so the user's own Brave stays open. The MCP starts a new browser on its next call.

If the call fails with "Could not find Google Chrome executable", the MCP config has lost its Brave path. It needs `--isolated --executablePath "/Applications/Brave Origin Nightly.app/Contents/MacOS/Brave Origin Nightly"` (`claude mcp get chrome-devtools`), and then a reconnect through `/mcp`.

## House style

Title 21pt bold · servings/time line (`8 servings (1 cup) | …`; the serving size comes from `nutrition.servingSize`, or from page text such as "Amount Per Serving (1 cup)", and is left out when only "1 serving" is known) in the Monospaced paragraph style (grey background), values bold, labels 11pt · grey rule · ingredients 14pt with bold amount, `#707070` name, italic `#CACACA` prep note · grey rule · numbered steps 15pt, `#CACACA` number, `#707070` text, a blank line between steps.

## Notes HTML import gotchas

The script already handles these. They matter only when you change the style.

- A font size survives import only when the span also names `font-family: -apple-system`. Without it, Notes drops sizes near body size.
- `<pre>` becomes the Monospaced paragraph style. Spans inside it use `font-family: ui-monospace`, or Notes stores Courier.
- `<h1>` does not become the Title paragraph style. No HTML does, so the title is body text at 21pt bold, which looks the same. The user can apply the real style with ⇧⌘T.
- To confirm a stored style, read the note's body through AppleScript. A copy of `NoteStore.sqlite` lags several seconds behind the edit.
- Do not edit a note by reading its body and setting it back. Notes exports `<tt>` without the font family, so the round trip stores Courier and drops the blank-line sizes. To change a note, render it again with the script, create the new note, and delete the old one.
