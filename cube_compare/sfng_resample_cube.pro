pro sfng_resample_cube, infile=infile, factor=factor,outfile=outfile
  
  cube=readfits(infile,hdr)

  cube_hrebin, data=cube,hdr_in=hdr,outfile=outfile,outcube=outcube, factor=factor, /quiet

end

