"""Repository rule and module extension for fetching AoC puzzle inputs.

Reads session cookie from AOC_SESSION env var or ~/.aoc_session.
Creates one external repo per year: @aoc_inputs_{year}//:day{n}.input
"""

def _aoc_year_inputs_impl(ctx):
    session = ctx.os.environ.get("AOC_SESSION", "")
    if not session:
        result = ctx.execute(["sh", "-c", "cat $HOME/.aoc_session 2>/dev/null || true"])
        session = result.stdout.strip()

    exported = []
    for day in ctx.attr.days:
        filename = "day%s.input" % day
        if session:
            ctx.download(
                url = "https://adventofcode.com/%s/day/%s/input" % (ctx.attr.year, day),
                output = filename,
                headers = {"Cookie": ["session=%s" % session]},
            )
        else:
            ctx.file(filename, "")
        exported.append(filename)

    ctx.file("BUILD.bazel", "exports_files(%s)\n" % repr(exported))

_aoc_year_inputs = repository_rule(
    implementation = _aoc_year_inputs_impl,
    attrs = {
        "year": attr.int(mandatory = True),
        "days": attr.int_list(mandatory = True),
    },
    environ = ["AOC_SESSION"],
)

_year_tag = tag_class(attrs = {
    "year": attr.int(mandatory = True),
    "days": attr.int_list(mandatory = True),
})

def _aoc_inputs_ext_impl(mctx):
    for mod in mctx.modules:
        for tag in mod.tags.year:
            _aoc_year_inputs(
                name = "aoc_inputs_%s" % tag.year,
                year = tag.year,
                days = tag.days,
            )

aoc_inputs_ext = module_extension(
    implementation = _aoc_inputs_ext_impl,
    tag_classes = {"year": _year_tag},
    environ = ["AOC_SESSION"],
)
