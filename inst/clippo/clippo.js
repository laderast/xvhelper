function get_selected_fields(id){
  sele = Reactable.getState(id).selected
  dat = Reactable.getState(id).data
  out_arr = sele.map(i => dat[i].entity_field)
  out_string = "c('" + out_arr.join("','") + "')";
  navigator.clipboard.writeText(out_string).then(
     function(x){
      alert("Selected fields copied to clipboard");
      }
    )
}
