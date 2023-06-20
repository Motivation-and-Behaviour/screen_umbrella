import os

from PyPDF2 import PdfReader, PdfWriter
from reportlab.lib.pagesizes import letter
from reportlab.lib.units import inch
from reportlab.pdfbase.pdfmetrics import getAscentDescent
from reportlab.pdfgen import canvas

# list your pdf files here
# now each value is a tuple with the title and a description
pdfs = {
    "supplementary_files/Supplementary File 1 - Search Strategy.pdf": (
        "Supplementary File 1 - Search Strategy",
        "The search strategies used in each database",
    ),
    "supplementary_files/Supplementary File 3 - Effect Size Codebook.pdf": (
        "Supplementary File 3 - Effect Size Codebook",
        "Generated codebook for the dataset.",
    ),
    "supplementary_files/Supplementary File 6 - Education Outcomes.pdf": (
        "Supplementary File 6 - Education Outcomes",
        "Additional education outcomes which did not meet certainty criteria.",
    ),
    "supplementary_files/Supplementary File 7 - Health-related Outcomes.pdf": (
        "Supplementary File 7 - Health-related Outcomes",
        "Additional health-related outcomes which did not meet certainty criteria.",
    ),
    "supplementary_files/Supplementary File 8 - Included Studies.pdf": (
        "Supplementary File 8 - Included Studies",
        "References for the included studies.",
    ),
    "supplementary_files/Supplementary File 9 - Effect Characteristics.pdf": (
        "Supplementary File 9 - Effect Characteristics",
        "Descriptive table for the included effects.",
    ),
}

tmp_output = PdfWriter()  # Temporarily store pages here
final_output = PdfWriter()  # Final output

toc = []  # table of contents list

page_num = 1  # track page numbers
for pdf, (title, description) in pdfs.items():
    # create a new PDF with Reportlab for title
    title_pdf = f"{title}.pdf"
    title_text = f"{title}"
    c = canvas.Canvas(title_pdf, pagesize=letter)
    width, height = letter
    c.setFont("Helvetica", 20)
    c.drawString(inch, height - 100, title_text)
    c.setFont("Helvetica-Bold", 14)
    c.drawString(inch, height - 130, "Description:")
    c.setFont("Helvetica", 14)
    c.drawString(inch + 90, height - 130, description)  # add the description
    c.save()

    # add the title page to the temp output
    with open(title_pdf, "rb") as f:
        page_num += 1  # increment for the title page
        pdf_title = PdfReader(f)
        tmp_output.add_page(pdf_title.pages[0])
        toc.append((page_num, title_text))  # add to the TOC only when new title starts

    # add the source pdf pages to the temp output
    with open(pdf, "rb") as f:
        pdf_file = PdfReader(f)
        for p in range(len(pdf_file.pages)):
            tmp_output.add_page(pdf_file.pages[p])
        page_num += len(pdf_file.pages)  # increment by the number of pages in the pdf

    # remove the temporary title PDF
    os.remove(title_pdf)

# Now create a table of contents at the beginning
toc_pdf = "toc.pdf"
c = canvas.Canvas(toc_pdf, pagesize=letter)
width, height = letter
c.setFont("Helvetica", 20)
c.drawString(inch, height - (inch * 1.5), "Table of Contents")
c.setFont("Helvetica", 16)
y = height - (inch * 1.5) - 50  # Leave space from top of page for first line
for page, text in toc:
    line = f"Page {page}: {text}"
    c.drawString(inch, y, line)
    y = y - (getAscentDescent("Helvetica", 16)[0] + 10)  # 10 for line spacing
c.save()

# Add the table of contents to the final output
with open(toc_pdf, "rb") as f:
    pdf_toc = PdfReader(f)
    final_output.add_page(pdf_toc.pages[0])

# remove the temporary TOC PDF
os.remove(toc_pdf)

# Add all other pages after TOC in final output
for i in range(len(tmp_output.pages)):
    final_output.add_page(tmp_output.pages[i])

# Finally, write the output file
with open("supplementary_files/Combined Supplementary Files.pdf", "wb") as f:
    final_output.write(f)