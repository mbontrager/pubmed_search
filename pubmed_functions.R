# PUBMED functions

# Get PUBMED data given search elements
get.pubmed.data = function(search.elements){
    require(RISmed)
    res = EUtilsSummary(search.elements)
    res_records = EUtilsGet(res)
    res = data.frame(PMID(res_records),
                           YearPubmed(res_records),          
                           Title(res_records),
                           ArticleTitle(res_records),
                           AbstractText(res_records),
                           Affiliation(res_records), 
                     stringsAsFactors = FALSE)
    
    colnames(res) = c("PMID",
                      "Year",
                      "Title",
                      "ArticleTitle",
                      "AbstractText",
                      "Affiliation")
    return(res)
}
