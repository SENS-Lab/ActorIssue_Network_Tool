# - Original -

# -Libraries-
library(shiny)
library(shinydashboard)
library(visNetwork)
library(igraph)
library(tidyverse)

# -Data-
actoractorEdgelist <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_aa_edgelist.csv"), header=TRUE)
actorissueEdgelist <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_ai_edgelist.csv"), header=TRUE)
issueissueEdgelist <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/ii_edgelist_weights.csv"), header=TRUE)
actordescription <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_actorattributes.csv"), header=TRUE)
issuedescription <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_issueattributes.csv"), header=TRUE)

# -Variables-
uniqueOrganizations <- actordescription[,2]
uniquePolicies <- issuedescription[,2]
uniqueElements <- c(uniqueOrganizations, uniquePolicies)

nodeEdgeFrom <- c( 
  actoractorEdgelist[,1],
  actorissueEdgelist[,1],
  issueissueEdgelist[,1]
)

nodeEdgeTo <- c(
  actoractorEdgelist[,2],
  actorissueEdgelist[,2],
  issueissueEdgelist[,2]
)

# -Tab 1 Nodes/Edges-
allNodes <- data.frame(
  id = uniqueElements, 
  label = uniqueElements, 
  group = c(
    rep("Organization", length(uniqueOrganizations)), 
    rep("Issue", length(uniquePolicies))
  )
)

allEdges <- data.frame(
  from = nodeEdgeFrom, 
  to = nodeEdgeTo,
  color = c(
    rep("gray", length(actoractorEdgelist[,1])), 
    rep("turquoise", length(actorissueEdgelist[,1])),
    rep("lightgreen", length(issueissueEdgelist[,1]))
  ),
  width = c(
    rep(0.5, length(actoractorEdgelist[,1])), 
    rep(0.5, length(actorissueEdgelist[,1])),
    issueissueEdgelist[,3][1:length(issueissueEdgelist[,3])]
  ),
  selectionwidth = c(
    rep(0.5, length(actoractorEdgelist[,1])), 
    rep(0.5, length(actorissueEdgelist[,1])),
    issueissueEdgelist[,3][1:length(issueissueEdgelist[,3])]
  ),
  id = paste(nodeEdgeFrom, nodeEdgeTo, sep="-")
)

# -Media-
dataURL = a("Data Entry Form", target="_blank", href="https://osu.az1.qualtrics.com/jfe/form/SV_cBxbSrIhRLx77lY")
channnelURL = a("Youtube Channel", target="_blank", href="https://www.google.com/")
coverVidURL = "https://www.youtube.com/embed/a7h6gI-yNpo"
imageURL = "https://students.cfaes.ohio-state.edu/sites/ap/files/events/SENR_CFAES.jpg"
legendURL = "https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/NoS_Tool_Legend.png"
bioText = ""

# -Shiny UI Variables-
abbrevNames <- c("N/A", actordescription[,2], issuedescription[,2])
fullNames <- c("N/A", actordescription[,1], issuedescription[,1])
names(abbrevNames) <- fullNames

abbrevNames2 <- c("N/A", actordescription[,2])
fullNames2 <- c("N/A", actordescription[,1])
names(abbrevNames2) <- fullNames2

# -Shiny UI-
ui <- dashboardPage(
  dashboardHeader(title = "Network of Stakeholders", titleWidth = 250),
  dashboardSidebar(
    
    # Tabs List
    sidebarMenu(id = "sidebar", style = "position: fixed; width: 225px;",
      menuItem("Home", tabName = "coverpage"),
      menuItem("Networks", tabName = "networkstab"),
      menuItem("Actor Profiles", tabName = "actorprofiletab")
    ),
    
    # Cover Page
    conditionalPanel(style = "position:fixed; width:225px; margin-top:125px;", condition = "input.sidebar == 'coverpage'"
      # <Placeholder Code>
    ),
    
    # Network Tab
    conditionalPanel(style = "position:fixed; width:225px; margin-top:125px;", condition = "input.sidebar == 'networkstab'",
      selectInput("filter", "Select Organization or Issue:", c(("N/A"), abbrevNames), selected = "N/A"),
      checkboxInput("checkOrg", "Show Organizations"),
      checkboxInput("checkIss", "Show Issues"),
      hr(),
      actionButton("reset", "Reset Networks")
    ),
    
    # Actor Profile Tab
    conditionalPanel(style = "position:fixed; width:225px; margin-top:125px;", condition = "input.sidebar == 'actorprofiletab'",
      selectInput("filterClone", "Select Organization or Issue:", c(("N/A"), abbrevNames2), selected = "")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
    tabItems(
      tabItem(tabName = "coverpage",
        # -Row 1-
        fluidRow(
          titlePanel(div(h1("Network of Stakeholders", align = "center"), 
          tags$div(style = "display: flex; justify-content: center", tags$img(src = imageURL, width = "50%", height = "50%"))))
        ),
        fluidRow(
          column(8, align = "center", offset = 2, box(title = "About Network of Stakeholders", width = 12, "<Paragraph>"))
        ),
        fluidRow(
          column(width = 6, box(title = "Network Analysis FAQ", width = NULL, "<Paragraph>")),
          column(width = 6, box(tags$iframe(src = coverVidURL, width = "100%", height = 300), width = NULL, height = 325))
        ),
        fluidRow(
          column(8, align = "center", offset = 2, box(title = "Links", width = NULL, HTML(paste(dataURL, channnelURL, sep = '<br/>'))))
        ),
        fluidRow(
          column(8, align = "center", offset = 2, box(title = "Bios", width = NULL, bioText))
        ),
        fluidRow(
          column(width = 6, align = "center", box(tags$iframe(src = coverVidURL, width = "100%", height = 300), width = NULL, height = 325))
        ),
      ),
      tabItem(tabName = "networkstab",
        
        # -Row 2-
        fluidRow(
          column(width = 3, align = "center",
                 box(title = "Instructions: Filtered Network", width = NULL, "This network only displays all connected nodes to the selected organization/issue. This effectively shows a sub-network of the network above (Full Network)."),
                 box(title = "Description of Selected Node/Edge", width = NULL, htmlOutput('description2')),
                 tags$h1("Legend", width = "50%", height = "50%", align = "center"),
                 tags$img(src = legendURL, width = "75%", height = "75%", align = "center")
          ),
          box(width = 6, height = 850, visNetworkOutput("network_proxy_tab2", height = 800)),
          box(title = "Filtered Network Table", width = 3, DT::dataTableOutput('table2')),
        ),
        
        # -Row 1-
        fluidRow(
          column(width = 3, align = "center",
                 box(title = "Instructions: Full Network", width = NULL, "This network is a 'full-sandbox' network that shows every organization and issue, and all connections that exist between them. Clicking on any node will select that organization/issue in the drop down menu."),
                 box(title = "Description of Selected Node/Edge", width = NULL, htmlOutput('description1')),
                 tags$h1("Legend", width = "50%", height = "50%", align = "center"),
                 tags$img(src = legendURL, width = "75%", height = "75%", align = "center")
          ),
          box(width = 6, height = 850, visNetworkOutput("network_proxy_tab1", height = 800)),
          box(title = "Full Network Table", width = 3, DT::dataTableOutput('table1')),
        ),
        
        # -Row 3-
        fluidRow(
          column(width = 3, align = "center",
                 box(title = "Instructions: Recommended Partners", width = NULL, "This network requires an organization to be selected from the drop down menu. This network shows all issues connected to the selected organization (from the drop down menu), and it shows all organizations connected to that set of issues (regardless of their connection to the selected organization). The network can be further filtered to only include organizations connected to a certain issue, which can be selected in the drop down menu below. The table on the right displays the amount of issues an organization and the selected organization have in common (Frequency), and it can be sorted by selecting the column header."),
                 box(title = "Description of Selected Node/Edge", width = NULL, htmlOutput('description3')),
                 box(width = NULL, selectInput("issueFilter", "Filter Issue :", "N/A", selected = "N/A")),
                 box(width = NULL, selectInput("orgFilter", "Filter Organization :", "N/A", selected = "N/A")),
                 tags$h1("Legend", width = "50%", height = "50%", align = "center"),
                 tags$img(src = legendURL, width = "75%", height = "75%", align = "center")
          ),
          box(width = 6, height = 850, visNetworkOutput("network_proxy_tab3", height = 800)),
          box(title = "Recommended Partners Table", width = 3, DT::dataTableOutput('table3'))
        )
      ),
      tabItem(tabName = "actorprofiletab",
        column(width = 5,
          box(title = "Image", width = NULL, htmlOutput('logo')),
          box(title = "Coordination Delegate", width = NULL, htmlOutput('coordinatedelegate')),
          box(title = "Actor Description", width = NULL, htmlOutput('actordescription')),
          box(title = "Actor Type", width = NULL, htmlOutput('actortype')),
          box(title = "Actor Website Hyperlinks", width = NULL, uiOutput('actorwebsite')),
          box(title = "Last Updated Date", width = NULL, htmlOutput('lastupdated'))
        ),
        box(width = 6, title = "Recommended Partners Table", DT::dataTableOutput('table4')),
        visNetworkOutput("network_proxy_tab4", height = 0)
      )
    )
  )
)

# -Shiny Server-
server <- function(input, output, session) {
  # --- Sidebar Tab 2 ---
  
  # -Tab 1 Graph-
  output$network_proxy_tab1  <- renderVisNetwork({
    
    # -Create Filtered Node Set-
    nodes1 <- allNodes
    if(!input$checkOrg) nodes1 <- nodes1[nodes1$id %in% uniquePolicies, ]
    if(!input$checkIss) nodes1 <- nodes1[nodes1$id %in% uniqueOrganizations, ]
    
    visNetwork(nodes1, allEdges, background = "white") %>%
      
      # -Node Groups-
      visGroups(groupname = "Organization", color = list(border = "blue", background = "blue", highlight = "black")) %>%
      visGroups(groupname = "Issue", color = list(border = "green", background = "green", highlight = "black")) %>%
      
      # -Current Node/Edge Variables-
      visEvents(select = "function(data) {
      Shiny.onInputChange('current_node_id1', data.nodes);
      Shiny.onInputChange('current_edges_id1', data.edges);
      ;}")  %>%
      
      #visLegend(addEdges = legendEdges1) %>%
      
      # -Modifications-
      visNodes(size = 10) %>%
      visEdges(smooth = list(enabled = TRUE, type = "horizontal")) %>%
      visIgraphLayout(randomSeed = 1, layout = "layout_in_circle") %>%
      visInteraction(dragNodes = FALSE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical", hover = FALSE)) %>%
      visConfigure(enabled = FALSE) # DEVS
  })
  
  # -Tab 2 Graph-
  output$network_proxy_tab2 <- renderVisNetwork({
    if(identical(!input$filter == "N/A", !input$filter == ""))
    {
      # -Create Filtered Node Set-
      connectedNodesA <- allNodes[allNodes$id %in% currentConnectedNodes$N, ]
      selectedNodeB <- data.frame(
        id = input$filter, 
        label = input$filter, 
        group = "Selected"
      )
      
      nodes2 <- rbind(connectedNodesA, selectedNodeB)
      
      if(!input$checkOrg) nodes2 <- nodes2[nodes2$id %in% uniquePolicies, ]
      if(!input$checkIss) nodes2 <- nodes2[nodes2$id %in% uniqueOrganizations, ]
      
      visNetwork(nodes2, allEdges, background = "white") %>%
        
        # -Node Groups-
        visGroups(groupname = "Organization", color = list(border = "blue", background = "blue", highlight = "black")) %>%
        visGroups(groupname = "Issue", color = list(border = "green", background = "green", highlight = "black")) %>%
        visGroups(groupname = "Selected", color = list(border = "yellow", background = "yellow", highlight = "black")) %>%
        
        # -Current Node/Edge Variables-
        visEvents(select = "function(data) {
        Shiny.onInputChange('current_node_id2', data.nodes);
        Shiny.onInputChange('current_edges_id2', data.edges);
        ;}")  %>%
        
        # -Modifications-
        visNodes(size = 10) %>%
        visEdges(smooth = list(enabled = TRUE, type = "horizontal")) %>%
        visIgraphLayout(randomSeed = 1, layout = "layout_nicely") %>%
        visInteraction(dragNodes = FALSE) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical")) %>%
        visConfigure(enabled = FALSE) # DEVS
    }
  })
  
  # - Tab 3 Graph-
  output$network_proxy_tab3 <- renderVisNetwork({
    if(identical(!input$filter == "N/A", !input$filter == "", input$filter %in% uniqueOrganizations))
    {
      # -Create Filtered Node Set-
      connectedNodesa <- allNodes[allNodes$id %in% currentConnectedNodes$N, ]
      connectedIssuesA <- NULL
      
      if(!input$issueFilter == "N/A") connectedIssuesA <- connectedNodesa[connectedNodesa$id %in% input$issueFilter, ] 
      else connectedIssuesA <- connectedNodesa[connectedNodesa$id %in% uniquePolicies, ] 
      
      orgsWithIssuesb <- vector() # Vector of all Instances of Organizations with Issues in [connectedIssuesA]
      for(x in 1:length(connectedIssuesA$id))
      {
        orgsWithIssuesb <- append(orgsWithIssuesb, actorissueEdgelist[connectedIssuesA$id[x] == actorissueEdgelist[2], 1])
      }
      
      if(!input$orgFilter == "N/A") orgsWithIssuesb <- orgsWithIssuesb[orgsWithIssuesb %in% input$orgFilter]
      OrgByIssueCount <- as.data.frame(table(orgsWithIssuesb)) # For Graph 3 Table
      
      orgsWithIssuesb <- unique(orgsWithIssuesb) # [orgsWithIssuesb] filters to unique Instances of Organizations
      orgsWithIssuesb <- orgsWithIssuesb[orgsWithIssuesb != input$filter]
      
      orgNodesWithIssuesB <- data.frame( # Organization Nodes
        id = orgsWithIssuesb,   
        label = orgsWithIssuesb,
        group = "Organization"
      )
      
      selectedNodeC <- data.frame( # Selected Node
        id = input$filter, 
        label = input$filter, 
        group = "Selected"
      )
      
      connectedNodesGraph3$A <- connectedIssuesA$id
      connectedNodesGraph3$B <- orgNodesWithIssuesB$id
      nodes3 <- rbind(connectedIssuesA, orgNodesWithIssuesB, selectedNodeC)
      
      if(!input$checkOrg) nodes3 <- nodes3[nodes3$id %in% uniquePolicies, ]
      if(!input$checkIss) nodes3 <- nodes3[nodes3$id %in% uniqueOrganizations, ]
      
      # -Create New Edge Set-
      connectedOrgEdges <- connectedNodesa[connectedNodesa$id %in% uniqueOrganizations, ]
      unconnectedOrgEdges <- orgNodesWithIssuesB[!(orgNodesWithIssuesB$id %in% connectedOrgEdges$id), ]
      
      newEdges <- data.frame(
        from = c(rep(input$filter, length(connectedOrgEdges$id)+length(unconnectedOrgEdges$id)), actorissueEdgelist[,1]),
        to = c(connectedOrgEdges$id, unconnectedOrgEdges$id, actorissueEdgelist[,2]),
        color = c(
          rep("darkgreen", length(connectedOrgEdges$id)),
          rep("red", length(unconnectedOrgEdges$id)),
          rep("lightgreen", length(actorissueEdgelist[,1]))
        )
      )
      
      # -Table-
      temp <- OrgByIssueCount[OrgByIssueCount$orgsWithIssuesb %in% unconnectedOrgEdges$id,]
      actorNameCol <- actordescription[actordescription$actor_list %in% temp$orgsWithIssuesb, 1:2]
      temp <- merge(temp, actorNameCol, by.x = "orgsWithIssuesb", by.y = 'actor_list', all = TRUE)
      tableDataFrame$table3 <- temp[, c(1,3,2)]
      
      # -Graph-
      visNetwork(nodes3, newEdges, background = "white") %>%
        
        # -Node Groups-
        visGroups(groupname = "Organization", color = list(border = "blue", background = "blue", highlight = "black")) %>%
        visGroups(groupname = "Issue", color = list(border = "green", background = "green", highlight = "black")) %>%
        visGroups(groupname = "Selected", color = list(border = "yellow", background = "yellow", highlight = "black")) %>%
        
        # -Current Node/Edge Variables-
        visEvents(select = "function(data) {
      Shiny.onInputChange('current_node_id3', data.nodes);
      Shiny.onInputChange('current_edges_id3', data.edges);
      ;}")  %>%
        
        # -Modifications-
        visNodes(size = 10) %>%
        visEdges(smooth = list(enabled = TRUE, type = "horizontal")) %>%
        visIgraphLayout(randomSeed = 1, layout = "layout_in_circle") %>%
        visInteraction(dragNodes = FALSE) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical"), manipulation = TRUE) %>%
        visConfigure(enabled = FALSE) # DEVS
    }
  })
  
  # -Table-
  output$table1 <- DT::renderDataTable(DT::datatable(tableDataFrame$table1, colnames = c("Abbrev.","Name","Type"), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 11)))
  output$table2 <- DT::renderDataTable(DT::datatable(tableDataFrame$table2, colnames = c("Abbrev.","Name","Type"), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 11)))
  output$table3 <- DT::renderDataTable(DT::datatable(tableDataFrame$table3, colnames = c("Abbrev.","Name", "Gaps"), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 11, order = list(2, 'desc'))))
  output$table4 <- DT::renderDataTable(DT::datatable(tableDataFrame$table4, colnames = c("Abbrev.","Name", "Gaps"), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 11, order = list(2, 'desc'))))
  
  # Variables
  currentConnectedNodes <- reactiveValues(N = NULL, N2 = NULL) # Connected Nodes
  tableDataFrame <- reactiveValues(table1 = NULL, table2 = NULL, table3 = NULL)
  connectedNodesGraph3 <- reactiveValues(A = NULL, B = NULL) # A: Issues, B: Organizations
  
  # Inputs
  resetCheck <- reactive({ list(input$reset) })
  
  # Set: [currentConnectedNodes]
  observe({
    if(identical(length(input$filter) == 1, !input$filter == "N/A")) {
      connectedNodesTo <- nodeEdgeTo[nodeEdgeFrom %in% input$filter]
      connectedNodesFrom <- nodeEdgeFrom[nodeEdgeTo %in% input$filter]
      currentConnectedNodes$N <- unique(c(connectedNodesTo, connectedNodesFrom))
    }
    else {
      currentConnectedNodes$N <- NULL
    }
  })
  
  # Set: [tableDataFrame$table1]
  observe({
    actorNameCol <- actordescription[actordescription$actor_list %in% allNodes$id, c(1:2,4)]
    issueNameCol <- issuedescription[issuedescription$abbr_issue %in% allNodes$id, 1:2]
    if(nrow(issueNameCol) != 0) issueNameCol['type'] <- NA
    colnames(issueNameCol) <- c('ï..name', 'actor_list', 'type')
    temp <- bind_rows(actorNameCol, issueNameCol)
    
    tableDataFrame$table1 <- temp[, c(2,1,3)]
  })
  
  # Set: [tableDataFrame$table2]
  observe({
    if(identical(length(input$filter) == 1, !input$filter == "N/A")) {
      nodesA <- allNodes[allNodes$id %in% currentConnectedNodes$N, ]
      nodesB <- data.frame(id = input$filter, label = input$filter, group = "Selected")
      nodesAB <- rbind(nodesB, nodesA)
      
      nodesAB <- subset(nodesAB, select = -c(label, group))
      actorNameCol <- actordescription[actordescription$actor_list %in% nodesAB$id, c(1:2,4)]
      issueNameCol <- issuedescription[issuedescription$abbr_issue %in% nodesAB$id, 1:2]
      if(nrow(issueNameCol) != 0){
        issueNameCol['type'] <- NA
        colnames(issueNameCol) <- c('ï..name', 'actor_list', 'type')
      } 
      
      temp <- bind_rows(actorNameCol, issueNameCol)
      tableDataFrame$table2 <- temp[, c(2,1,3)]
    }
    else {
      tableDataFrame$table2 <- NULL
    }
  })
  
  # Set: [tableDataFrame$table3]
  observe({
    if(input$filter == "N/A")
    {
      tableDataFrame$table3 <- NULL
    }
  })
  
  # Update: Graph 1 from Filter Box
  observe({
    visNetworkProxy("network_proxy_tab1") %>%
      visUnselectAll() %>%
      visSelectNodes(id = input$filter) %>%
      visFocus(id = input$filter, scale = 1, locked = FALSE, animation = list(duration = 500))
  })
  
  # Update: Filter Box from Graph 1
  observe({
    if(is.null(input$current_node_id1)) updateSelectInput(session, "filter", selected = "N/A")
    else updateSelectInput(session, "filter", selected = input$current_node_id1)
  })
  
  # Update: Graph 1 Description Box
  output$description1 <- renderUI({
    if(!is.null(input$current_node_id1))
    {
      if(input$current_node_id1 %in% uniquePolicies) issuedescription[issuedescription$abbr_issue == input$current_node_id1, 3]
      else actordescription[actordescription$actor_list == input$current_node_id1, 3]
    }
    else if(!is.null(input$current_edges_id1)) 
    {
      node1 <- sub("\\_.*", "", input$current_edges_id1)
      node2 <- sub("\\_.*", "", substr(input$current_edges_id1, nchar(node1)+2, nchar(input$current_edges_id1)))
      
      newList <- data.frame(gsub('\\s+', '', (unlist(issueissueEdgelist[, 1], use.names=FALSE))),
                            gsub('\\s+', '', (unlist(issueissueEdgelist[, 2], use.names=FALSE))), issueissueEdgelist[, 4])
      newList[newList[1] == node1 & newList[2] == node2, 3]
    }
  })
  
  # Update: Graph 2 Description Box
  output$description2 <- renderUI({
    if(!is.null(input$current_node_id2))
    {
      if(input$current_node_id2 %in% uniquePolicies) issuedescription[issuedescription$abbr_issue == input$current_node_id2, 3]
      else actordescription[actordescription$actor_list == input$current_node_id2, 3]
    }
    else if(!is.null(input$current_edges_id2)) 
    {
      node1 <- sub("\\_.*", "", input$current_edges_id2)
      node2 <- sub("\\_.*", "", substr(input$current_edges_id2, nchar(node1)+2, nchar(input$current_edges_id2)))
      
      newList <- data.frame(gsub('\\s+', '', (unlist(issueissueEdgelist[, 1], use.names=FALSE))),
                            gsub('\\s+', '', (unlist(issueissueEdgelist[, 2], use.names=FALSE))), issueissueEdgelist[, 4])
      newList[newList[1] == node1 & newList[2] == node2, 3]
    }
  })
  
  # Update: Graph 3 Description Box 
  output$description3 <- renderUI({
    if(!is.null(input$current_node_id3))
    {
      if(input$current_node_id3 %in% uniquePolicies) issuedescription[issuedescription$abbr_issue == input$current_node_id3, 3]
      else actordescription[actordescription$actor_list == input$current_node_id3, 3]
    }
    else if(!is.null(input$current_edges_id3)) 
    {
      node1 <- sub("\\_.*", "", input$current_edges_id3)
      node2 <- sub("\\_.*", "", substr(input$current_edges_id3, nchar(node1)+2, nchar(input$current_edges_id3)))
      
      newList <- data.frame(gsub('\\s+', '', (unlist(issueissueEdgelist[, 1], use.names=FALSE))),
                            gsub('\\s+', '', (unlist(issueissueEdgelist[, 2], use.names=FALSE))), issueissueEdgelist[, 4])
      newList[newList[1] == node1 & newList[2] == node2, 3]
    }
  })
  
  # Update: Graph 3 Issue/Organization Filter Box 
  observeEvent(input$filter,{
    updateSelectInput(session, "issueFilter", choices = "N/A", selected = "N/A")
    updateSelectInput(session, "orgFilter", choices = "N/A", selected = "N/A")
  })
  # Update: Graph 3 Issue/Organization Filter Box N/A Reset
  observe({
    if(input$issueFilter == "N/A") updateSelectInput(session, "issueFilter", choices = c("N/A", connectedNodesGraph3$A))
    if(input$orgFilter == "N/A") updateSelectInput(session, "orgFilter", choices = c("N/A", connectedNodesGraph3$B))
  })
  
  # Observe: Graph 1
  observeEvent(input$table1_cell_clicked, {
    a <- tableDataFrame$table1[input$table1_rows_selected,1]
    if(length(a) == 1)
    {
      visNetworkProxy("network_proxy_tab1") %>%
        visFocus(id = a, scale = 1, locked = FALSE, animation = list(duration = 500))
    }
  })
  # Observe: Graph 2
  observeEvent(input$table2_cell_clicked, {
    a <- tableDataFrame$table2[input$table2_rows_selected,1]
    if(length(a) == 1)
    {
      visNetworkProxy("network_proxy_tab2") %>%
        visFocus(id = a, scale = 2, locked = FALSE, animation = list(duration = 500))
    }
  })
  # Observe: Graph 3
  observeEvent(input$table3_cell_clicked, {
    a <- tableDataFrame$table3[input$table3_rows_selected,1]
    if(length(a) == 1)
    {
      visNetworkProxy("network_proxy_tab3") %>%
        visFocus(id = a, scale = 1, locked = FALSE, animation = list(duration = 500))
    }
  })
  
  # Observe: Reset 
  observeEvent(resetCheck(), {
    updateSelectInput(session, "filter", selected = "N/A")
    updateSelectInput(session, "checkOrg", selected = TRUE)
    updateSelectInput(session, "checkIss", selected = TRUE)
    visNetworkProxy("network_proxy_tab1") %>%
      visFit(animation = list(duration=650))
  })
  
  # --- Sidebar Tab 3 ---
  
  # Match filter inputs between Sidebar Tab 2&3
  observeEvent(input$filter, {
    if(any(uniqueOrganizations==input$filter)) {
      updateSelectInput(session, "filterClone", selected = input$filter)
    }
  })
  
  output$coordinatedelegate <- renderUI({
    actordescription[actordescription$actor_list %in% input$filterClone, "coordination_delegate"]
  })
  
  output$actordescription <- renderUI({
    actordescription[actordescription$actor_list %in% input$filterClone, "desc"]
  })
  
  output$actortype <- renderUI({
    actordescription[actordescription$actor_list %in% input$filterClone, "type"]
  })
  
  output$actorwebsite <- renderUI({
    if(input$filterClone != "N/A")
    {
      link <- actordescription[actordescription$actor_list %in% input$filterClone, "link"]
      tagList(a(link, href=link, style = "word-wrap: break-word;"))
    }
    else
    {}
  })
  
  output$lastupdated <- renderUI({
    actordescription[actordescription$actor_list %in% input$filterClone, "last_updated"]
  })
  
  output$logo <- renderText({
    link <- actordescription[actordescription$actor_list %in% input$filterClone, "logo"]
    paste(c('<img width = 100% height = 100% src="', link, '">'))
  })
  
  output$network_proxy_tab4 <- renderVisNetwork({
    if(identical(!input$filterClone == "N/A", !input$filterClone == "", input$filterClone %in% uniqueOrganizations))
    {
      
      # -Create Filtered Node Set-
      connectedNodesa <- allNodes[allNodes$id %in% currentConnectedNodes$N2, ]
      connectedIssuesA <- NULL
      
      if(!input$issueFilter == "N/A") connectedIssuesA <- connectedNodesa[connectedNodesa$id %in% input$issueFilter, ] 
      else connectedIssuesA <- connectedNodesa[connectedNodesa$id %in% uniquePolicies, ] 
      
      orgsWithIssuesb <- vector() # Vector of all Instances of Organizations with Issues in [connectedIssuesA]
      for(x in 1:length(connectedIssuesA$id))
      {
        orgsWithIssuesb <- append(orgsWithIssuesb, actorissueEdgelist[connectedIssuesA$id[x] == actorissueEdgelist[2], 1])
      }
      
      if(!input$orgFilter == "N/A") orgsWithIssuesb <- orgsWithIssuesb[orgsWithIssuesb %in% input$orgFilter]
      OrgByIssueCount <- as.data.frame(table(orgsWithIssuesb)) # For Graph 3 Table
      
      orgsWithIssuesb <- unique(orgsWithIssuesb) # [orgsWithIssuesb] filters to unique Instances of Organizations
      orgsWithIssuesb <- orgsWithIssuesb[orgsWithIssuesb != input$filterClone]
      
      orgNodesWithIssuesB <- data.frame( # Organization Nodes
        id = orgsWithIssuesb,   
        label = orgsWithIssuesb,
        group = "Organization"
      )
      
      selectedNodeC <- data.frame( # Selected Node
        id = input$filterClone, 
        label = input$filterClone, 
        group = "Selected"
      )
      
      connectedNodesGraph3$A <- connectedIssuesA$id
      connectedNodesGraph3$B <- orgNodesWithIssuesB$id
      nodes3 <- rbind(connectedIssuesA, orgNodesWithIssuesB, selectedNodeC)
      
      if(!input$checkOrg) nodes3 <- nodes3[nodes3$id %in% uniquePolicies, ]
      if(!input$checkIss) nodes3 <- nodes3[nodes3$id %in% uniqueOrganizations, ]
      
      # -Create New Edge Set-
      connectedOrgEdges <- connectedNodesa[connectedNodesa$id %in% uniqueOrganizations, ]
      unconnectedOrgEdges <- orgNodesWithIssuesB[!(orgNodesWithIssuesB$id %in% connectedOrgEdges$id), ]
      
      newEdges <- data.frame(
        from = c(rep(input$filterClone, length(connectedOrgEdges$id)+length(unconnectedOrgEdges$id)), actorissueEdgelist[,1]),
        to = c(connectedOrgEdges$id, unconnectedOrgEdges$id, actorissueEdgelist[,2]),
        color = c(
          rep("darkgreen", length(connectedOrgEdges$id)),
          rep("red", length(unconnectedOrgEdges$id)),
          rep("lightgreen", length(actorissueEdgelist[,1]))
        )
      )
      
      # -Table-
      temp <- OrgByIssueCount[OrgByIssueCount$orgsWithIssuesb %in% unconnectedOrgEdges$id,]
      actorNameCol <- actordescription[actordescription$actor_list %in% temp$orgsWithIssuesb, 1:2]
      temp <- merge(temp, actorNameCol, by.x = "orgsWithIssuesb", by.y = 'actor_list', all = TRUE)
      tableDataFrame$table4 <- temp[, c(1,3,2)]
      
      # -Graph-
      visNetwork(nodes3, newEdges, background = "white") %>%
        
        # -Node Groups-
        visGroups(groupname = "Organization", color = list(border = "blue", background = "blue", highlight = "black")) %>%
        visGroups(groupname = "Issue", color = list(border = "green", background = "green", highlight = "black")) %>%
        visGroups(groupname = "Selected", color = list(border = "yellow", background = "yellow", highlight = "black")) %>%
        
        # -Current Node/Edge Variables-
        visEvents(select = "function(data) {
      Shiny.onInputChange('current_node_id3', data.nodes);
      Shiny.onInputChange('current_edges_id3', data.edges);
      ;}")  %>%
        
        # -Modifications-
        visNodes(size = 10) %>%
        visEdges(smooth = list(enabled = TRUE, type = "horizontal")) %>%
        visIgraphLayout(randomSeed = 1, layout = "layout_in_circle") %>%
        visInteraction(dragNodes = FALSE) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical"), manipulation = TRUE) %>%
        visConfigure(enabled = FALSE) # DEVS
    }
  })
  
  # Set: [currentConnectedNodes]
  observe({
    if(identical(length(input$filterClone) == 1, !input$filterClone == "N/A")) {
      connectedNodesTo <- nodeEdgeTo[nodeEdgeFrom %in% input$filterClone]
      connectedNodesFrom <- nodeEdgeFrom[nodeEdgeTo %in% input$filterClone]
      currentConnectedNodes$N2 <- unique(c(connectedNodesTo, connectedNodesFrom))
    }
    else {
      currentConnectedNodes$N2 <- NULL
    }
  })
}
# -Run-
shinyApp(ui = ui, server = server)