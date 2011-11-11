let rec create_float5x3array _ = (
  let vec = create_array 3 0.0 in
  let array = create_array 5 vec in
  array.(1) <- create_array 3 0.0;
  array.(2) <- create_array 3 0.0;
  array.(3) <- create_array 3 0.0;
  array.(4) <- create_array 3 0.0;
  array
)
in

let rec create_pixel _ =
  let m_rgb = create_array 3 0.0 in
  let m_isect_ps = create_float5x3array() in
  let m_sids = create_array 5 0 in
  let m_cdif = create_array 5 false in
  let m_engy = create_float5x3array() in
  let m_r20p = create_float5x3array() in
  let m_gid = create_array 1 0 in
  let m_nvectors = create_float5x3array() in
  (m_rgb, m_isect_ps, m_sids, m_cdif, m_engy, m_r20p, m_gid, m_nvectors)

in
(
    let x = create_pixel() in
    let y = create_pixel() in
    ()
)
