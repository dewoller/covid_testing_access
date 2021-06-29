
# make max_dist/divisor labels of the form xkm-ykm, starting from 0 going to max_dist by divisor
make_labels = function( max_dist = 60, divisor = 5, descriptor = 'km') {

  chunks = floor(max_dist / divisor - .000001) +1
  p2= seq_len(chunks)*divisor
  p1= p2-divisor

  paste0(p1,descriptor,'-',p2,descriptor)
}



