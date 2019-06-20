# [UI Implementation Repo](https://github.com/Eve-ning/osutools_ui)

# Difficulty Discussion

There are several models used by VSRGs, most of them can be classified under **Static Model**. I'm approaching this with **Dynamic Model**.

## Static Model

A **Static Model** is where there's minimal consideration on the direction of the chart.

Essentially you can calculate the **moving average nps** of the chart from left to right, it'll be the same if you did it from right to left.

As a rule of thumb, all static models must output only the following 2 columns `offsets` and `value`

## Dynamic Model

Dynamic Model makes use of **Static Models** as its base to power **trackers**. You'd get much more realistic results as you're mimicking the player.

### Trackers

Trackers are vectors that are usually initialized on 0 at the start of the chart. Trackers are used to store a stress value that would change according to its `path` and `obstacles`.

You can think of `path` and `obstacles` as if in real life. The `stress` value will `spike` when an `obstacle` is struck, but will gradually `decay` after time.

This is the **Dynamic Model**.

# Compartments

As explained, we are using a **Dynamic Model**, however, it requires **Static Models** to generate its `obstacles`.

Here are the main models:
- Density
- Jack Inverse
- Motion Bias

## Density

This is mainly for "readability" difficulty.

Taking it at face value, that means how hard is it to process what you're seeing on the screen.

We don't want to go too in-depth to avoid slowing down the difficulty calculation, so let's consider only the basics. *"The screen will be hard to read because of its density."*

Objects:
- Note
- LNoteHead
- LNoteBody
- LNoteTail

### LNoteBody

LNoteBody is a bit of an anomaly, however, we shouldn't disregard its power to increase difficulty. We will leave this in to allow more flexibility in calculating difficulty.

## Jack Inverse

Jack Inverse is the subset of `diffBroadcast`. We can easily grab the jacks from there

Jack Inverse is basically the **inverse value of the distance between jacks**. It is inverted to amplify smaller distances and muffle larger ones.

### Conditional Capping 

However, it's important to cap the Jack Inverse value as mini-jacks can easily skyrocket this value.

## Motion Bias

Motion Bias is essentially a hardcoded table of parameter to describe how hard is it to do a pattern with a certain fingering style.

In other words, we calculate this via finger motion instead of what column goes to what column.

This will be really hard to calculate correctly, but thankfully it's flexible enough to allow Tensorflow to try to decrease its loss.

### Jack Inverse Overlapping

As both **Motion Bias** and **Jack Inverse** are dealing with patterns, we will exclude jacks from **Motion Bias**.

# Small Diff Correction

**Jack Inverse** and **Motion Bias** will value `diffs` that are very low in value, and possibly inflate them too much.

## Jack Inverse
Despite *mini-jacks* being arguably easy to execute, we will **assume** that its justified.

## Motion Bias
The biggest issue here would be graces, small `diffs` are essentially just graced chords, so we shouldn't weight that too highly.

In other words, we'd have a weighting system that supresses `diffs` that are too low.

# Expected Code Workflow

```{r}
# Parse the chart normally
chart <- chartParse("path/to/osu")

# Extended Parse of the chart for models
chart.ext <- chartExtract(chart, ...)

# Static Models
jck <- model.jackInv(chart.ext) 
mtn <- model.mtn(chart.ext)
dns <- model.density(chart.ext)

# Dynamic Model (stressSim)
ss <- chart %>% stressSim(sm.1, sm.2, ..., sm.n)

# Combine
diff <- ss %>% diffCalc(...)
```



