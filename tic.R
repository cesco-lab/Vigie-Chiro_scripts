do_package_checks()

if (ci_on_travis()) {
  get_stage("before_install") %>%
  add_code_step(
    # Ubuntugis already in travis
    system("sudo apt-get install --yes libudunits2-dev libproj-dev libgeos-dev libgdal-dev"))

  do_pkgdown()
}
