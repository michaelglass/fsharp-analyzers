/// <summary>
/// Checks inline suppression comments that silence specific analyzer diagnostics.
/// A marker <c>// {CODE}:ok</c> can sit on the construct's first line, on the line
/// immediately above it, anywhere in the contiguous <c>//</c> comment block directly
/// above it, or on any later line of the construct's own range.
/// </summary>
/// <remarks>
/// <para><b>One marker, one site.</b> A marker binds only the construct it is written
/// for, never one that merely encloses it. When several of a rule's sites could hold
/// the same marker line, it binds the one it is nearest to, in this order:</para>
/// <list type="number">
/// <item>a site whose first line the marker is on (a trailing comment on that line);</item>
/// <item>a site the marker is written above (the line above it, or its comment block);</item>
/// <item>the innermost site whose range holds the marker: the one starting nearest
/// above it, then the shortest.</item>
/// </list>
/// <para>So a marker in an inner <c>try</c>'s handler, or above an inner <c>try</c>,
/// suppresses that inner <c>try</c> and not the outer one that contains it. Sites that
/// tie on all three (two flagged literals on one line, say) are bound together: no
/// placement can tell them apart.</para>
/// </remarks>
module MichaelGlass.FSharp.Analyzers.Suppression

open System
open FSharp.Compiler.Text

/// <summary>Returns true when the line is a whole-line <c>//</c> comment.</summary>
let private isCommentLine (line: string) =
    line.TrimStart().StartsWith("//", StringComparison.Ordinal)

/// <summary>
/// How a marker on 0-based line <paramref name="markerIdx"/> sits relative to
/// <paramref name="site"/>, as a rank where lower binds first; <c>None</c> when the
/// marker cannot bind the site at all.
/// </summary>
let private placementRank (lines: string array) (site: range) (markerIdx: int) : (int * int * int) option =
    let startIdx = site.StartLine - 1
    let endIdx = site.EndLine - 1

    // The contiguous comment block above the site. It ends at the first blank
    // line, line of code, or the start of the file, which is what keeps a marker
    // that has drifted away from its construct from suppressing something else.
    let inCommentBlockAbove () =
        markerIdx < startIdx - 1
        && seq { markerIdx .. startIdx - 1 }
           |> Seq.forall (fun idx -> isCommentLine lines.[idx])

    if markerIdx = startIdx then
        Some(0, 0, 0)
    elif markerIdx = startIdx - 1 || inCommentBlockAbove () then
        Some(1, startIdx - markerIdx, 0)
    elif markerIdx > startIdx && markerIdx <= endIdx then
        Some(2, markerIdx - startIdx, endIdx - startIdx)
    else
        None

/// <summary>The sites one marker binds: those it can bind at the lowest rank.</summary>
let private bound (lines: string array) (sites: range list) (markerIdx: int) : range list =
    let ranked =
        sites
        |> List.choose (fun site -> placementRank lines site markerIdx |> Option.map (fun rank -> rank, site))

    match ranked with
    | [] -> []
    | _ ->
        let best = ranked |> List.map fst |> List.min
        ranked |> List.filter (fun (rank, _) -> rank = best) |> List.map snd

/// <summary>
/// Builds the suppression test for one rule over one file.
/// </summary>
/// <param name="sourceText">The source text of the file being analyzed.</param>
/// <param name="code">The analyzer diagnostic code (e.g. "MGA-WILDCARD-001").</param>
/// <param name="sites">
/// Every construct of this rule in the file: the ones it flags and, where the rule
/// inspects constructs it then passes, those too. A marker belongs to the nearest
/// of these, so a site left out could let its marker reach one that encloses it.
/// </param>
/// <returns>A predicate: true when a marker binds the diagnostic at that range.</returns>
let suppressor (sourceText: ISourceText) (code: string) (sites: range list) : range -> bool =
    let marker = code + ":ok"

    let lines =
        Array.init (sourceText.GetLineCount()) (fun idx -> sourceText.GetLineString idx)

    let suppressed =
        lines
        |> Array.indexed
        |> Array.filter (fun (_, line) -> line.Contains(marker, StringComparison.Ordinal))
        |> Array.toList
        |> List.collect (fun (idx, _) -> bound lines sites idx)

    fun range -> suppressed |> List.exists (fun site -> Range.equals site range)
