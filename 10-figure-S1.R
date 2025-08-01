library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Note: You can change fontname to any font you like, but it must be installed and available on your system
# Common options: Helvetica, Arial, Times-Roman, Courier
p <- grViz("
digraph state_diagram {
  graph [layout = dot, rankdir = LR, concentrate = false, splines = spline]
  node [shape = ellipse, style = filled, fillcolor = lightblue, fontname = Rosario, fontsize = 12]
  edge [arrowhead = vee, arrowsize = 0.5]

  {
    rank = same;
    Start;
  }

  {
    rank = same;
    \"No Events\"; \"Other Intracranial Bleeding\"; \"Extracranial Bleeding\"; \"Ischemic Stroke\"; \"Hemorrhagic Stroke\";
  }

  {
    rank = same;
    \"No Disability\"; \"Mild Disability\"; \"Moderate Disability\"; \"Severe Disability\";
  }

  {
    rank = same;
    \"New Cycle\";
  }

  {
    rank = same;
    Death;
  }

  // Transitions from Start
  Start -> \"No Events\"
  Start -> \"Other Intracranial Bleeding\"
  Start -> \"Extracranial Bleeding\"
  Start -> \"Ischemic Stroke\"
  Start -> \"Hemorrhagic Stroke\"
  Start -> \"Death\"

  // No Events → New Cycle
  \"No Events\" -> \"New Cycle\"

  // Each bleeding/stroke event → all disabilities and death
  \"Other Intracranial Bleeding\" -> \"No Disability\"
  \"Other Intracranial Bleeding\" -> \"Mild Disability\"
  \"Other Intracranial Bleeding\" -> \"Moderate Disability\"
  \"Other Intracranial Bleeding\" -> \"Severe Disability\"
  \"Other Intracranial Bleeding\" -> Death

  \"Extracranial Bleeding\" -> \"No Disability\"
  \"Extracranial Bleeding\" -> \"Mild Disability\"
  \"Extracranial Bleeding\" -> \"Moderate Disability\"
  \"Extracranial Bleeding\" -> \"Severe Disability\"
  \"Extracranial Bleeding\" -> Death

  \"Ischemic Stroke\" -> \"No Disability\"
  \"Ischemic Stroke\" -> \"Mild Disability\"
  \"Ischemic Stroke\" -> \"Moderate Disability\"
  \"Ischemic Stroke\" -> \"Severe Disability\"
  \"Ischemic Stroke\" -> Death

  \"Hemorrhagic Stroke\" -> \"No Disability\"
  \"Hemorrhagic Stroke\" -> \"Mild Disability\"
  \"Hemorrhagic Stroke\" -> \"Moderate Disability\"
  \"Hemorrhagic Stroke\" -> \"Severe Disability\"
  \"Hemorrhagic Stroke\" -> Death

  // Disabilities → New Cycle
  \"No Disability\" -> \"New Cycle\"
  \"Mild Disability\" -> \"New Cycle\"
  \"Moderate Disability\" -> \"New Cycle\"
  \"Severe Disability\" -> \"New Cycle\"
}
")

# Save as PDF
rsvg_pdf(charToRaw(export_svg(p)), file = "figures/figure_S1.pdf")

# # Create PNG version using magick if necessary
# pdf_image <- magick::image_read_pdf("figures/figure_S1.pdf", density = 300)
# magick::image_write(pdf_image,
#                     path = "figures/figure_S1.png",
#                     format = "png",
#                     density = 300)

