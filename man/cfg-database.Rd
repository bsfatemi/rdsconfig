% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfg-database.R
\name{cfg-database}
\alias{cfg-database}
\alias{userIndex}
\alias{orgIndex}
\alias{get_polaris_config}
\alias{get_polaris_admins}
\alias{org_auth}
\alias{get_stores}
\alias{plIndex}
\alias{plAppDataIndex}
\alias{get_brands_index}
\alias{erpSysIndex}
\alias{get_product_skus}
\title{Functions that Query the Database}
\usage{
userIndex()

orgIndex(extra_cols = FALSE)

get_polaris_config()

get_polaris_admins()

org_auth(org, .api)

get_stores()

plIndex()

plAppDataIndex()

get_brands_index()

erpSysIndex()

get_product_skus(org, store)
}
\arguments{
\item{extra_cols}{A logical indicating if new function columns should be returned}

\item{org}{org guid}

\item{.api}{API to use for auth}

\item{store}{store uuid}
}
\description{
Functions that Query the Database
}
\section{Functions}{
\itemize{
\item \code{userIndex()}: users

\item \code{orgIndex()}: users

\item \code{get_polaris_config()}: get polaris config

\item \code{get_polaris_admins()}: get polaris admins

\item \code{org_auth()}: look up orgs twil credentials given org Id

\item \code{get_stores()}: Gets all Stores set in our database.

\item \code{plIndex()}: pipelines index

\item \code{plAppDataIndex()}: pipelines index for appdata

\item \code{get_brands_index()}: get index of brands

\item \code{erpSysIndex()}: pos system index

\item \code{get_product_skus()}: get list of products for a retail location

}}
