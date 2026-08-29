{
  writeShellApplication,
  ghostscript,
  exiftool,
}:

# Inverts the colors of a PDF and flattens it to greyscale, which makes
# scanned sheet music and similar documents readable on a dark background.
writeShellApplication {
  name = "invert-pdfs";

  runtimeInputs = [
    ghostscript
    exiftool
  ];

  text = ''
    for file in "$@"; do
      target=out/$(basename "''${file% Numbers.pdf}.pdf")

      echo "Converting from $file to $target"

      # Invert colors
      gs -o /tmp/invert-step1.pdf -sDEVICE=pdfwrite -dPDFFitPage \
        -c "{1 exch sub}{1 exch sub}{1 exch sub}{1 exch sub} setcolortransfer" \
        -f "$file"

      # Rasterize and convert to greyscale
      gs -sDEVICE=pdfimage8 -r1200 -dPDFFitPage -dDownScaleFactor=2 \
        -o "$target" -f /tmp/invert-step1.pdf

      # Ghostscript carries over metadata we don't want, so strip everything
      # except the title.
      exiftool -overwrite_original -all= -TagsFromFile @ -Title "$target"
    done
  '';
}
