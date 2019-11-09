do_package_checks()

if (ci_on_travis()) {
  get_stage("before_install") %>%
  add_code_step(
    system(
      "sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable --yes & \\
        sudo apt-get --yes --force-yes update -qq & \\
        sudo apt-get install --yes libudunits2-dev libproj-dev libgeos-dev libgdal-dev"))

  do_pkgdown()
}
