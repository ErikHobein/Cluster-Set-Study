# Load required packages ====
library(shiny)
library(rsconnect)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)
library(viridis)
library(metan)
library(SimplyAgree)
library(lme4)
library(lmerTest)
library(bmbstats)
library(gt)

# Data ====
data_test <- read_excel("data.xlsx", sheet = 1) 
data_1RM <- read_excel("data.xlsx", sheet = 2) 
data_VBT <- read_excel("data.xlsx", sheet = 3) 
data_performance <- read_excel("data.xlsx", sheet = 4)
data_fatigue <- read_excel("data.xlsx", sheet = 5)
data_jump <- read_excel("data.xlsx", sheet = 6)

# Data processing ====
data_VBT$Set <- as.factor(data_VBT$Set)

data_VL <- data_VBT %>%
  group_by(ID, Group, Session, Sex, Set) %>%
  reframe(
    VL = ((max(MPV) - last(MPV)) / max(MPV) * 100),
    MPV = mean(MPV),
  )

data_acute <- data_VL %>%
  group_by(ID, Group, Session, Sex) %>%
  reframe(
    MPV = mean(MPV),
    VL = mean(VL)
  ) %>%
  left_join(data_fatigue, by = c("ID", "Group", "Session", "Sex")) %>%
  mutate(
    CMJ_dif = - ((CMJ_pre_mean - CMJ_post_mean) / CMJ_pre_mean) * 100,
    RSI_mod_dif = - ((RSI_mod_pre_mean - RSI_mod_post_mean) / RSI_mod_pre_mean) * 100, 
    v70_dif = - ((v70_pre_mean - v70_post_mean) / v70_pre_mean) * 100,
    Lactate_dif = - (Lactate_pre - Lactate_post)
  )

data_acute <- data_acute %>%
  mutate(Session = as.factor(Session))


# define my theme for plots
mytheme <- theme_classic() +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "plain", hjust = 0.5),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.line = element_line(linewidth = 0.9),
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.key = element_rect(color = FALSE, fill = "grey"))

# Load data ====
data_test <- read_excel("data.xlsx", sheet = 1) 
data_1RM <- read_excel("data.xlsx", sheet = 2) 
data_VBT <- read_excel("data.xlsx", sheet = 3) 
data_performance <- read_excel("data.xlsx", sheet = 4)
data_fatigue <- read_excel("data.xlsx", sheet = 5)

# Define UI ====
ui <- fluidPage(
  navbarPage(
    "Cluster Set Study",
    
    # Performance Dashboard Tab
    tabPanel(
      "Performance Dashboard",
      sidebarPanel(
        width = 2,
        tags$h3("Input:"),
        selectInput("selected_id", "ID", choices = sort(unique(data_1RM$ID))), 
        selectInput("selected_time", "Time", choices = c("pre", "post"))
      ),
      mainPanel(
        width = 10,
        h2("Individual Profile"),
        p("Here you can find individual performance results obtained during the study's pre and post testing."),
        p("Use the dropdown menu on the left to select different subjects and time points."),
        tags$br(),
        h3("Load-Velocity Profile"),
        div(class = "plot-container", 
            style = "border: 1px solid #ddd; padding: 10px;",  
            plotlyOutput("Load_Velocity_plot", width = "100%", height = "600px")
        ),
        tags$br(),
        p("The graph above displays the relationship between load (kg) and mean propulsive velocity (MPV in m/s) during the 1RM test in the free-weight back squat."),
        p("The fitted regression line helps estimate the velocity at different loads, which is useful for strength training analysis and individualized training prescriptions."),
        h3("Performance Profile"),
        p("Here you find the individual results of the selected ID to the selected time point (10RM tests were only conducted during the pre-tests). The isometric force was measured on a smith machine with 90 degrees knee angle and a force plate. 
          v70 represents the velocity in the free-weight back squat with a load of 70% 1RM. CMJ, v70 and isometric force are the mean values of three attempts."),
        p("The graph displays the Percentiles in comparison to the other particpants of the same sex at the same time."),
        fluidRow(
          column(9,
                 div(class = "plot-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",  
                     plotOutput("circular_plot", width = "100%", height = "600px")
                 )
          ),
          column(3,
                 div(class = "table-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",
                     dataTableOutput("data_table", width = "100%", height = "600px"))
          ),
        )
      )
    ),
    
    # Validity VBT
    tabPanel(
      "Validity VBT",
      sidebarPanel(
        width = 2,
        tags$h3("Input:"),
        selectInput("selected_sex", "Sex", choices = c("both", "female", "male")),
        selectInput("selected_time2", "Time", choices = c("both", "pre", "post"))
      ),
      mainPanel(
        width = 10,
        h2("Validity VBT"),
        p("The aim in this section is to figure out, whether the estimation of velocity-based 1RM is aligning with the actual 1RM and how precise prescribing training intensity by using velocity is."),
        tags$br(),
        h3("Intensity-Velocity Relation"),
        p("Here you can see which mean propulsive velocity describes specific training intensities in the free-weight back squat. We got these data from the 1RM tests during the pre- and post-tests."),
        fluidRow(
          column(9,
                 div(class = "plot-container", 
                     style = "border: 1px solid #ddd; padding: 10px;", 
                     plotlyOutput("Intensity_Velocity_plot", width = "100%", height = "600px")
                 )
          ),
          column(3,
                 div(class = "table-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",
                     dataTableOutput("Intensity_Velocity_table", width = "100%", height = "600px"))
          ),
        ),
        tags$br(),
        h3("Bland-Altman Analysis"),
        p("Here you can choose between four different VBT based 1RM estimation models and see how much the calculated 1RM differs to the tested 1RM.
          All models are based on linear regression. General Model means, that a generalised terminal velocity of 0.32 m/s was used, while individual models used an individual observed terminal velocity for each participant during a strength endurance test with 60% 1RM.
          The two point model just fitted the regression through two points (30% and 70% 1RM while the full model fitted through all data points from 1RM test.
          For the BA-analysis you can see the dashed lines. This is the margin (+/- 5 kg) where the author defined apriori a acceptable fit. The LoA should lie inside this margin, when the models are accurate.
          (Note: filters are only available for sex and not time)"),
        tags$br(),
        div(
          class = "plot-container", 
          style = "border: 1px solid #ddd; padding: 10px;",
          tabsetPanel(
            tabPanel("General Model", plotlyOutput("ba_gen", width = "100%", height = "500px")),
            tabPanel("Individual Model", plotlyOutput("ba_ind", width = "100%", height = "500px")),
            tabPanel("v1RM Model", plotlyOutput("ba_v1RM", width = "100%", height = "500px")),
            tabPanel("General 2-Point Model", plotlyOutput("ba_2p_gen", width = "100%", height = "500px")),
            tabPanel("Individual 2-Point Model", plotlyOutput("ba_2p_ind", width = "100%", height = "500px")),
            tabPanel("v1RM 2-Point Model", plotlyOutput("ba_2p_v1RM", width = "100%", height = "500px"))
          )
        )
      )
    ),
    
    # Comparison CMJ Heights
    tabPanel(
      "Comparison CMJ Heights",
      sidebarPanel(
        width = 2,
        tags$h3("Input:"),
        selectInput("selected_sex4", "Sex", choices = c("both", "female", "male"))
      ),
      mainPanel(
        width = 10,
        h2("Comparison CMJ Heights"),
        p("It is well known that different calculations of the CMJ height result in different results. Implemented in most of the Force Plates are the metrics for flight time and concentric impulse.
        Therefore, here the outcomes these two jump height calculation methods are compared to provide coaches with the information of the typical bias they have to deal with by using/comparing these two different metrics.
        Compared are always the averages of three jumps like the data were collected in this study. All jumps evaluated in the study are added in this analysis."),
        h3("Bland-Altman Plot"),
        div(
          class = "plot-container", 
          style = "border: 1px solid #ddd; padding: 10px;",
          plotlyOutput("ba_jh", width = "100%", height = "500px")),
      )
    ),
    
    # Acute Effects Tab
    tabPanel(
      "Acute Effects",
      sidebarPanel(
        width = 2,
        tags$h3("Input:"),
        selectInput("selected_sex2", "Sex", choices = c("both", "female", "male")),
        selectInput("selected_session", "Session", choices = sort(unique(data_VBT$Session))),
      ),
      mainPanel(
        width = 10,
        h2("Acute Effects"),
        p("This section focuses on the acute outcomes of the training. Therefore performance, metabolic and subjective measures are used. You can see details about each training session in the table below."),
        div(class = "table-container", 
            style = "border: 1px solid #ddd; padding: 10px;",
            DTOutput("session_table", width = "100%")
        ),
        tags$br(),
        h3("Velocity Profiles"),
        p("The plot shows the velocities during each repetition during the training as mean and sd. The dashed line represents a velocity loss of 20% to the fastest repetition.
        Addtional information to the velocity metrics of the chosen session are presented in the table and boxplot below."),
        div(class = "plot-container", 
            style = "border: 1px solid #ddd; padding: 10px;",  
            plotlyOutput("Velocity_profiles", width = "100%", height = "400px")
        ),
        fluidRow(
          column(6,
                 div(class = "plot-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",  
                     plotlyOutput("Velocity_boxplots", width = "100%", height = "200px")
                 )
          ),
          column(6,
                 div(class = "table-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",  
                     DTOutput("Velocity_table", width = "100%", height = "200px"))
          )
        ),
        tags$br(),
        h3("Acute Fatigue"),
        p("Here you can choose between four different plots. These plots display their specific metric for every training session, but you can still choose between sexes on the side panel.
        The plots compare the mean velocity during the training, the velocity loss during the training, the CMJ height difference pre to post training and the CMJ reactive strength index modified (RSI mod.) difference pre to post training between groups."),
        div(
          class = "plot-container", 
          style = "border: 1px solid #ddd; padding: 10px;",
          tabsetPanel(
            tabPanel("MPV", plotlyOutput("plot_MPV", width = "100%", height = "500px")),
            tabPanel("Velocity Loss", plotlyOutput("plot_VL", width = "100%", height = "500px")),
            tabPanel("CMJ Difference", plotlyOutput("plot_CMJ", width = "100%", height = "500px")),
            tabPanel("RSI Mod. Difference", plotlyOutput("plot_RSI", width = "100%", height = "500px"))
          )
        ),
        tags$br(),
        h3("Correlation of Fatigue Parameters"),
        p("This is the correlation matrix between all fatigue parameters. 
        Each parameter was assessed in the 2.; 5.; 8. and 11. session and therefore the data of these four sessions are used."),
        div(class = "plot-container", 
            style = "border: 1px solid #ddd; padding: 10px;",  
            plotOutput("cor_matrix", width = "100%", height = "600px")
        ),
      )
    ),
    
    # Chronic Effects Tab
    tabPanel(
      "Chronic Effects",
      sidebarPanel(
        width = 2,
        tags$h3("Input:"),
        selectInput("selected_sex3", "Sex", choices = c("both", "female", "male")),
      ),
      mainPanel(
        width = 10,
        h2("Chronic Effects"),
        p("This section focuses on the training adaptations over the time of the study period."),
        h3("1RM Adaptation"),
        p("The plot shows the adaptation of the 1RM in the free-weight back squat pre to post training intervention for each group."),
        div(class = "plot-container", 
            style = "border: 1px solid #ddd; padding: 10px;",
            plotlyOutput("boxplot_with_spaghetti", width = "100%", height = "400px")
        ),
        h3("L-V Profile Adaptation"),
        p("The plot shows the adaptation of the load-velocity profiles during the 1RM tests for each group."),
        div(class = "plot-container", 
            style = "border: 1px solid #ddd; padding: 10px;",
            plotlyOutput("plot_VL_profiles", width = "100%", height = "600")
        ),
        tags$br(),
        h3("Summary Table"),
        p("The tables below show the mean and sd values for the different tests (left table) and the change of the score pre to post as absolute and standardized numbers (right table)."),
        fluidRow(
          column(6,
                 div(class = "table-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",  
                     DTOutput("Adaptation_table", width = "100%", height = "400px"))
          ),
          column(6,
                 div(class = "table-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",  
                     DTOutput("Adaptation_difference_table", width = "100%", height = "400px"))
          )
        ),
        tags$br(),
        h3("Performance Development"),
        p("You can choose between two plots. The performance development over the study period in the CMJ height or the velocity in the back squat with a load of 70% 1RM  (v70) are displayed.
        These variables were evaluated in th pre- and post-tests and monitored before every training session."),
        div(
          class = "plot-container", 
          style = "border: 1px solid #ddd; padding: 10px;",
          tabsetPanel(
            tabPanel("CMJ", plotlyOutput("CMJ_development", width = "100%", height = "400px")),
            tabPanel("v70", plotlyOutput("v70_development", width = "100%", height = "400px"))
          )
        )
      )
    ),
    
    # Study Information Tab
    tabPanel(
      "About the Study",
      fluidPage(
        h2("Cluster Set Study Overview"),
        p("This study investigates the acute and chronic effects of cluster set training in comparison to traditional set structures during maximum strength training in the free weight back squat."),
        p("Cluster set training is a form of resistance training, where additional intra-set rest periods are implemented between repetitions. These are thought to allow partial recovery and to maintain movement velocity."),
        h3("Key Objectives"),
        tags$ul(
          tags$li("Acute effects on movement velocity during the training sessions"),
          tags$li("Acute effects on fatigue measures directly after the training sessions"),
          tags$li("Chronic training adaptations for maximum strength, speed strength and power"),
          tags$li("Some smaller projects like the evaluation of the validity of load-velocity relationship in the free-weight back squat and comparement of CMJ jump height observed by flight-time and concentric impulse calculations"),
        ),
        h3("How to Use this web application?"),
        p("This web application does not represent the exact study outcomes and does not include any statistical tests."),
        p("It is thought to visualize the data from this study in a wide range to get several plots for specific training sessions, IDs or sexes."),
        p("Therefore, please use the dropdown menus on the left side of the different dashboards."),
        tags$br(),
        h3("Methods"),
        h4("Participants"),
        p("To participate, individuals had to meet the following criteria: (1) be aged between 18 and 35 years; (2) possess at least one year of strength training experience and a relative 1RM of ≥1.00 (females) or ≥1.25 (males) in the free-weight back squat; (3) have no current injuries or illnesses; (4) for female participants, not be pregnant; and (5) abstain from using substances prohibited by the World Anti-Doping Agency (WADA). 
          Participants were advised to maintain their usual dietary routines and avoid any additional resistance training throughout the study. The research received ethical approval from the Ethics Committee of the Faculty of Sports Science, Ruhr University Bochum (approval number: EKS V 2024_18). All procedures complied with the Declaration of Helsinki (WMA, 2024), except for database registration, and written informed consent was obtained from all participants prior to their involvement."),
        p("The plot below shows the relative strength of the participants in the free-weight back squat to the start of the intervention.
          The table summarizes the characteristics of the participants to the start of the study."),
        fluidRow(
          column(5,
                 div(class = "plot-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",  
                     plotOutput("plot_1RM_distribution")
                 )
          ),
          column(7,
                 div(class = "table-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",
                     gt_output("table_characteristics"))
          )
        ),
        h4("Study design"),
        p("This randomized controlled trial was part of a broader investigation examining the acute and chronic effects of CS training versus TS training. The intervention spanned six weeks, during which participants completed two training sessions per week, totaling 12 sessions. 
          A pre-test week was conducted before the intervention, consisting of two sessions: a familiarization session and a baseline testing session. Following the six-week training period, a post-test week was performed to assess changes in the relevant parameters. 
          Participants were stratified by sex and relative 1RM strength within their respective study cycle cohorts. Once matched, they were randomly assigned to either the CS or TS training groups. This stratification ensured an equitable distribution of strength levels and sex across the groups, minimizing bias and enhancing the validity of comparisons between the two training protocols."),
        h4("Procedures"),
        h5("Familiarization Session"),
        p("The familiarization session, held before the intervention, aimed to prepare participants for the study's testing and training protocols, reducing learning effects and injury risk. Before the session, body height and weight were measured following a 12-hour fast to ensure consistency. 
          Participants were then introduced into the standardized warm-up protocol used throughout every session and to key exercises, including the free-weight back squat, countermovement jump (CMJ), and isometric squat. Proper technique was demonstrated and supervised to ensure correct form. 
          Squat depth for the free-weight back squat was individually assessed by having participants squat as deeply as possible with 50% of their body weight while maintaining proper form. The deepest position was recorded using a haptic barrier for consistency in future sessions. 
          This individualized approach was adopted due to its practical relevance, recognizing that there are differences in individual mobility and muscle activation across participants, including variations between sexes. Participants were instructed to perform the eccentric phase of each squat in a controlled manner at their preferred velocity, while executing the concentric phase with maximum effort. 
          Bar velocity was monitored throughout all sessions using a linear position transducer (Vitruve encoder, SPEED4LIFTS S.L., Madrid, Spain) operating at a sampling frequency of 100 Hz. Velocity measurements were recorded as mean propulsive velocity (MPV). 
          Knee angles for the isometric squat were measured with a goniometer and set to approximately 90 degrees, aligning with evidence that this range corresponds to preferred angles during CMJ and squat jump performance. Additionally, a 10RM test was conducted for the bench press, leg curl, and single-arm cable row to determine individualized training loads for the intervention. 
          The test began with two warm-up sets at 50% and 60% of the estimated 1RM, followed by progressive maximal effort trials until technical or muscular failure. Rest periods of at least three minutes were provided between sets to ensure full recovery."),
        h5("Testing sessions (pre/post)"),
        p("The pre- and post-intervention testing sessions were conducted to assess key performance metrics, including explosive strength, maximum strength, speed strength, and strength endurance. Identical procedures were implemented during both sessions to evaluate the adaptations resulting from the six-week training intervention. 
          Explosive strength was measured using the CMJ, performed on force plates (ForceDecks, VALD Performance, Newstead, Australia). Participants completed three maximal CMJ trials, with a 20-second rest period between attempts. Jump height was calculated using the impulse-momentum method, and the average of the three trials was recorded for analysis. 
          Isometric maximal strength was assessed using a Smith machine (2SC Multipower, Technogym, Gambettola, Italy) combined with force plates to measure ground reaction forces. Participants performed three maximal effort isometric squats, each held for three seconds at a knee angle of approximately 90 degrees, as established during the familiarization session. 
          Two-minute rest intervals were provided between trials, and the highest ground reaction force across the three attempts was used for further analysis. To determine maximal dynamic strength, a one-repetition maximum (1RM) test was conducted in the free-weight back squat. The 1RM test followed a progressive loading protocol, beginning with 5 repetitions at 30% of the estimated 1RM, followed by 3 repetitions at 50% and 70% of the estimated 1RM, with rest periods of 2–3 minutes between sets. 
          Participants then performed a single repetition at 80% of the estimated 1RM with a 3-minute rest. Following these warm-up sets, single repetitions with progressively heavier loads were performed until the participant's 1RM was established. Rest periods of 3–5 minutes were provided between maximal attempts, and the 1RM was defined as the heaviest load lifted with proper technique. 
          Speed strength was evaluated by measuring bar velocity during three repetitions at 70% of the 1RM (v70). Participants completed the repetitions with maximum effort, and the average bar velocity across the three trials was recorded for analysis. Strength endurance was assessed by instructing participants to perform as many repetitions as possible at 60% of their 1RM. The total number of repetitions completed before failure was recorded to quantify muscular endurance under submaximal loads."),
        h5("Training sessions"),
        p("Each training session began with an assessment of participants' recovery status using the Short Recovery and Stress Scale (SRSS), followed by a standardized warm-up, including mobilization exercises and three maximal CMJ, measured as per the testing protocol. 
          Participants then performed one set of five repetitions at 30% and three repetitions at 70% of their 1RM. The average velocity during these lifts (v70) was recorded, and load adjustments were made based on velocity changes: a 5% adjustment for a change of 0.07–0.14 m/s, and a 10% adjustment for a change greater than 0.15 m/s. 
          These adjustments ensured optimal load progression based on pilot data. After load adjustment, participants completed the primary squat training protocol, using either the cluster set (CS) or traditional set (TS) method, with intensities increasing from 70% to 85% of their 1RM. The set and rep structure varied across sessions, from 3×8 at lower intensities to 4×3 at higher intensities. Each repetition was performed with maximum concentric effort and a controlled eccentric phase. 
          Thirty seconds post-squat session, participants performed another CMJ test to assess acute fatigue. In sessions 2, 5, 8, and 11, additional fatigue measures, including v70 (post 2 min), ratings of perceived exertion (RPE) (after every set), SRSS (post training, post 24 h, post 48 h), delayed onset muscle soreness (DOMS) (post 24 h, post 48 h), and blood lactate levels (post 3 min) were collected. 
          After the squat protocol, participants completed supplemental exercises, including bench press, leg curl, single-arm cable row, plank, and side plank, following a periodization scheme across the 12 sessions, to control the additional resistance training load."),
        tags$br(),
        h4("Contributors"),
        p("Ivan Jukic, Fabian Miltner, Alexander Ferrauti, Thimo Wiewelhove, Erik Hobein"),
        tags$br(),
        h4("Contact"),
        p("If you have any questions about the study, please get in contact:"),
        p("Erik Hobein: erik.hobein@rub.de")
      )
    ),
    # Impressum
    tabPanel(
      "Impressum",
      fluidPage(
        h4("Impressum"),
        p("Angaben gemäß § 5 TMG:"),
        p("Erik Hobein"),
        p("Grüner Weg 29"),
        p("58119 Hagen"),
        p("Kontakt:"),
        p("Telefon: +4915730116138"),
        p("E-Mail: erik.hobein@rub.de"),
        p("Website:  https://sport-erik-hobein.shinyapps.io/ClusterSetStudy/"),
        p("Haftungshinweis:"),
        p("Trotz sorgfältiger inhaltlicher Kontrolle übernehmen wir keine Haftung für die Inhalte externer Links. Für den Inhalt der verlinkten Seiten sind ausschließlich deren Betreiber verantwortlich. Es ist es ein Anliegen, die Seite aktuell zu halten. Eine Haftung oder Garantie für die Aktualität, Richtigkeit und Vollständigkeit der zur Verfügung gestellten Informationen und Daten ist jedoch ausgeschlossen."),
        p("Copyright:"),
        p("Alle Rechte vorbehalten. Ohne Zustimmung ist es verboten, textliche und fotografische Inhalte dieser Internetseite im ganzen oder in Teilen zu nutzen, zu vervielfältigen, zu verbreiten oder zu verwerten. Die Inhalte dürfen nicht verändert oder bearbeitet werden. Anschriften, Telefonnummern etc. dürfen nur zu privaten Zwecken verwendet werden. Gewerbliche oder ähnliche Nutzung ist untersagt.")
      )
    )
  )
)

# Define server function ====
server <- function(input, output, session) {
  # Load - Velocity Profile ====
  output$Load_Velocity_plot <- renderPlotly({
    selected_data <- data_1RM %>% 
      filter(ID == input$selected_id, Time == input$selected_time)
    
    if (nrow(selected_data) > 0) {
      # Fit a linear model
      model <- lm(MPV ~ Load, data = selected_data)
      
      # Extract model details
      r_squared <- round(summary(model)$r.squared, 4)
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      equation <- paste0("y = ", round(intercept, 4), " + ", round(slope, 4), "x")
      
      plot <- ggplot(selected_data, aes(x = Load, y = MPV)) +
        geom_point(color = "#17365c", size = 3) + 
        geom_smooth(method = "lm", se = FALSE, color = "#17365c", linewidth = 1) + 
        labs(x = "Load [kg]", y = "MPV [m/s]") +
        scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
        scale_y_continuous(breaks = seq(0, 1.8, by = 0.2), limits = c(0, 1.8)) +
        mytheme +
        annotate("text", x = 150, y = 1.70, label = paste("R² =", r_squared), size = 5, hjust = 0, color = "#17365c") +
        annotate("text", x = 150, y = 1.50, label = equation, size = 5, hjust = 0, color = "#17365c")
      
      # Convert ggplot to plotly for interactivity
      ggplotly(plot)
    } else {
      plot_ly() %>%
        layout(title = "No data for selected variables")
    }
  })
  
  # circular bar plot with performance profile ====
  output$circular_plot <- renderPlot({
    selected_data <- data_test %>%
      group_by(Time, Sex) %>%
      reframe(
        ID = ID,
        Time = Time,
        `CMJ` = percent_rank(CMJ_mean) * 100,
        `Isometric Force` = percent_rank(Isometric_force_mean) * 100, 
        `1RM Squat` = percent_rank(`1RM`) * 100, 
        `rel. 1RM` = percent_rank(`rel_1RM`) * 100, 
        `v70` = percent_rank(v70_mean) * 100, 
        `Max. Reps` = percent_rank(max_reps) * 100, 
        `10RM Bench-Press` = percent_rank(`10RM_Bench_Press`) * 100, 
        `10RM Leg-Curl` = percent_rank(`10RM_Leg_Curl`) * 100, 
        `10RM Single-Arm-Row` = percent_rank(`10RM_single_arm_row`) * 100
      ) %>%
      # Apply filter after percentile calculation but before pivoting
      filter(ID == input$selected_id, Time == input$selected_time) %>%
      pivot_longer(
        cols = starts_with("CMJ") | starts_with("Isometric") | starts_with("1RM") | starts_with("rel.") | starts_with("v70") | starts_with("Max.") | starts_with("10RM"),
        names_to = "Measurement",
        values_to = "Percentiles"
      )
    
    if (nrow(selected_data) > 0) {
      
      plt <- ggplot(selected_data) +
        # Custom panel grid lines
        geom_hline(
          aes(yintercept = y), 
          data.frame(y = seq(0, 100, by = 20)), 
          color = "gray80"
        ) + 
        # Bar chart
        geom_col(
          aes(
            x = reorder(str_wrap(Measurement, 5), Percentiles),
            y = Percentiles,
            fill = Percentiles
          ),
          position = "dodge2",
          show.legend = TRUE,
          alpha = 0.8
        ) +
        # Labels inside bars
        geom_label(
          aes(
            x = reorder(str_wrap(Measurement, 5), Percentiles),
            y = Percentiles - 5,  # Adjust so labels fit inside
            label = round(Percentiles)
          ),
          color = "black",
          fill = "white",
          size = 3,
          fontface = "bold",
          family = "Comic Sans MS",
          show.legend = FALSE
        ) +
        # Make it circular!
        coord_polar() +
        # Annotate custom scale inside plot
        annotate("text", x = 9.7, y = 105, label = "100", size = 4) +
        annotate("text", x = 9.7, y = 85, label = "80", size = 4) +
        annotate("text", x = 9.7, y = 65, label = "60", size = 4) +
        annotate("text", x = 9.7, y = 45, label = "40", size = 4) +
        annotate("text", x = 9.7, y = 25, label = "20", size = 4) +
        annotate("text", x = 9.7, y = 5, label = "0", size = 4) +
        # Scale y axis so bars don’t start in the center
        scale_y_continuous(
          limits = c(-10, 120),  # Adjusted for better spacing
          expand = c(0, 0),
          breaks = seq(0, 100, by = 20)
        ) + 
        # Fill colors and legend adjustments
        scale_fill_viridis_c(
          name = "Percentile",
          option = "C",
          begin = 0,
          end = 0.8,
          guide = guide_colorbar(barwidth = 15, barheight = 0.75)
        ) +
        # Final styling
        theme_void() +
        theme(
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "gray12", size = 14),
          legend.position = "bottom"
        )
      
      
      plt  
    } else {
      plot.new() %>%
        layout(title = "No data for selected variables")
    }
  }) 
  
  # Performance Data Table ====
  output$data_table <- renderDataTable({
    # Filter Data
    selected_data <- data_test %>%
      filter(ID == input$selected_id, Time == input$selected_time)
    
    if (nrow(selected_data) == 0) {
      return(NULL)  
    }
    
    # Select parameters of relevance
    valid_columns <- c("Sex", "Body_weight", "CMJ_mean", "Isometric_force_mean", 
                       "1RM", "rel_1RM", "v70_mean", 
                       "max_reps", "10RM_Bench_Press", 
                       "10RM_Leg_Curl", "10RM_single_arm_row")
    
    # Filter available parameters
    selected_data <- selected_data %>%
      select(all_of(intersect(valid_columns, colnames(selected_data))))
    
    # Define names of parameters
    new_column_names <- c("Sex","Body Weight [kg]", "CMJ [cm]", "Isometric Force [N/BW]", 
                          "1RM Squat [kg]", "rel. 1RM [kg/BW]", "v70 [m/s]", 
                          "Max. Reps", "10RM Bench Press [kg]", 
                          "10RM Leg Curl [kg]", "10RM Single Arm Row [kg]")
    
    colnames(selected_data) <- new_column_names[1:ncol(selected_data)]
    
    # Round data to 2 decimal places
    selected_data <- selected_data %>%
      mutate(across(where(is.numeric), ~ round(., 2)))
    
    # Transpose the data frame
    transposed_data <- as.data.frame(t(selected_data))
    colnames(transposed_data) <- "Value"
    
    # Add a column for parameters
    transposed_data$Parameter <- rownames(transposed_data)
    transposed_data <- transposed_data[, c("Parameter", "Value")]
    rownames(transposed_data) <- NULL
    
    # Create the datatable output
    datatable_output <- datatable(
      transposed_data,
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        scrollY = '425px', 
        searching = FALSE,  
        paging = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      class = 'cell-border stripe',
      rownames = TRUE,  
      colnames = c("", "")  # Remove column names
    )
    
    # Output of table
    datatable_output
  })
  
  # Intensity - Velocity Plot ====
  output$Intensity_Velocity_plot <- renderPlotly({
    selected_data <- data_1RM %>%
      group_by(ID, Time) %>%
      arrange(ID, Load) %>%
      mutate(Load_percent = (`Load` / last(`Load`)) * 100)
    
    if (input$selected_time2 != "both") {
      selected_data <- selected_data %>% filter(Time == input$selected_time2)
    }
    
    if (input$selected_sex != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex)
    }
    
    # Linear model and prediction interval
    model1 <- lm(MPV ~ Load_percent, data = selected_data)
    predict_data <- data.frame(Load_percent = seq(0, 100, by = 5)) 
    predict_P <- predict(model1, interval = "prediction", newdata = predict_data)
    predict_C <- predict(model1, interval = "confidence", newdata = predict_data)
    
    prediction_data <- cbind(
      predict_data,
      fit = predict_P[, 1],         
      lwr_PI = predict_P[, 2],       
      upr_PI = predict_P[, 3],       
      lwr_CI = predict_C[, 2],       
      upr_CI = predict_C[, 3]
    )
    
    # R-sqaured-value and regression calculations
    summary_model <- summary(model1)
    r_squared <- round(summary_model$r.squared, 4)
    equation <- paste0("y = ", round(coef(model1)[1], 4), round(coef(model1)[2], 4), "x")
    
    selected_data$ID <- as.factor(selected_data$ID)
    
    # Plot 
    plot <- ggplot(selected_data, aes(x = Load_percent, y = `MPV`)) +
      geom_point(aes(color = ID), size = 3, alpha = .8) +
      geom_line(data = prediction_data, aes(x = Load_percent, y = fit), linetype = "solid", color = "black", linewidth = 1) +
      geom_line(data = prediction_data, aes(x = Load_percent, y = lwr_CI), linetype = "dotted", color = "black", linewidth = 0.5) +
      geom_line(data = prediction_data, aes(x = Load_percent, y = upr_CI), linetype = "dotted", color = "black", linewidth = 0.5) +
      geom_line(aes(x = Load_percent, y = lwr_PI), data = prediction_data, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_line(aes(x = Load_percent, y = upr_PI), data = prediction_data, linetype = "dashed", color = "black", linewidth = 0.5) +
      scale_color_viridis(discrete = TRUE, option = "C", direction = -1, begin = 0, end = 0.8) +
      scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100)) +
      scale_y_continuous(breaks = seq(0, 2.2, by = 0.2), limits = c(0,2.2)) +
      xlab("Load [% of 1RM]") +
      ylab("MPV [m/s]") +
      mytheme +
      annotate("text", x = 80, y = 2.0, label = paste("R² = ", r_squared), size = 5, hjust = 0) +
      annotate("text", x = 80, y = 1.8, label = equation, size = 5, hjust = 0)
    
    # Convert ggplot to plotly for interactivity
    ggplotly(plot)
    
  })
  
  # Velocity to Intensity ====
  output$Intensity_Velocity_table <- renderDataTable({
    selected_data <- data_1RM %>%
      group_by(ID, Time) %>%
      arrange(ID, Load) %>%
      mutate(Load_percent = (`Load` / last(`Load`)) * 100)
    
    if (input$selected_time2 != "both") {
      selected_data <- selected_data %>% filter(Time == input$selected_time2)
    }
    
    if (input$selected_sex != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex)
    }
    
    # Linear model and prediction interval
    model1 <- lm(MPV ~ Load_percent, data = selected_data)
    predict_data <- data.frame(Load_percent = seq(0, 100, by = 5)) 
    predict_P <- predict(model1, interval = "prediction", newdata = predict_data)
    predict_C <- predict(model1, interval = "confidence", newdata = predict_data)
    
    prediction_data <- cbind(
      predict_data,
      fit = predict_P[, 1],         
      lwr_PI = predict_P[, 2],       
      upr_PI = predict_P[, 3],       
      lwr_CI = predict_C[, 2],       
      upr_CI = predict_C[, 3]
    )
    
    # Calculate CI and PI
    table_data <- prediction_data %>%
      mutate(
        `Velocity [m/s]` = round(fit, 2),
        `95% CI [m/s]` = paste0(round(lwr_CI, 2), " - ", round(upr_CI, 2)),
        `95% PI [m/s]` = paste0(round(lwr_PI, 2), " - ", round(upr_PI, 2))
      ) %>%
      select(
        `Load [% of 1RM]` = Load_percent,
        `Velocity [m/s]`,
        `95% CI [m/s]`,
        `95% PI [m/s]`
      )
    
    # Create the datatable output
    datatable_output <- datatable(
      table_data,
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        scrollY = '475', 
        searching = FALSE,  
        paging = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      class = 'cell-border stripe'
    )
    
    datatable_output
    
  })
  
  
  # Compute Bland-Altman Data ====
  ba_data <- reactive({
    
    # Transform data and calculate model 1RM ====
    
    # === General + Individual model based on full Load/MPV ===
    lm_all <- data_1RM %>%
      group_by(ID, Time) %>%
      nest() %>%
      mutate(model = map(data, ~ lm(MPV ~ Load, data = .)),
             intercept = map_dbl(model, ~ coef(.x)[1]),
             slope = map_dbl(model, ~ coef(.x)[2])) %>%
      select(ID, Time, intercept, slope)
    
    # === Terminal velocity during real 1RM ===
    v_1RM_df <- data_1RM %>%
      group_by(ID, Time) %>%
      summarise(v_1RM = last(MPV), .groups = "drop")
    
    # === Align and merge all info ===
    data_test_aligned <- data_test %>%
      left_join(lm_all, by = c("ID", "Time")) %>%
      left_join(v_1RM_df, by = c("ID", "Time"))
    
    # === General constant ===
    gen_terminal_velocity <- 0.32
    
    # === Compute predicted 1RMs (general, individual, v_1RM) ===
    data_test_aligned <- data_test_aligned %>%
      mutate(
        Gen_Predicted_1RM = (gen_terminal_velocity - intercept) / slope,
        Ind_Predicted_1RM = (Min_v - intercept) / slope,
        v1RM_Predicted_1RM = (v_1RM - intercept) / slope
      )
    
    # === Two-point model based on 30–70% loads ===
    data_two_point <- data_test %>%
      group_by(ID, Time) %>%
      summarise(
        Load = list(c(Load_30, Load_70)),
        MPV = list(c(v30_peak, v70_peak)),
        .groups = "drop"
      ) %>%
      mutate(
        data = map2(Load, MPV, ~ tibble(Load = .x, MPV = .y)),
        model = map(data, ~ lm(MPV ~ Load, data = .)),
        intercept_2p = map_dbl(model, ~ coef(.x)[1]),
        slope_2p = map_dbl(model, ~ coef(.x)[2])
      ) %>%
      select(ID, Time, intercept_2p, slope_2p)
    
    # === Merge two-point model and calculate predicted 1RMs ===
    data_test_aligned <- data_test_aligned %>%
      left_join(data_two_point, by = c("ID", "Time")) %>%
      mutate(
        Two_Point_Gen_Predicted_1RM = (gen_terminal_velocity - intercept_2p) / slope_2p,
        Two_Point_Ind_Predicted_1RM = (Min_v - intercept_2p) / slope_2p,
        Two_Point_v1RM_Predicted_1RM = (v_1RM - intercept_2p) / slope_2p
      )
    
    # === Final summary dataframe with all predicted 1RMs ===
    data_BA <- data_test_aligned %>%
      select(
        ID, Time, Sex, `1RM`, 
        Gen_Predicted_1RM, Ind_Predicted_1RM, v1RM_Predicted_1RM,
        Two_Point_Gen_Predicted_1RM, Two_Point_Ind_Predicted_1RM, Two_Point_v1RM_Predicted_1RM
      ) %>%
      rename(Tested_1RM = `1RM`)
    
    # Apply filters for selected sex
    if (input$selected_sex != "both") {
      data_BA <- data_BA %>% filter(Sex == input$selected_sex)
    }
    
    return(data_BA)
    
  })
  
  # Function to create Bland-Altman plot
  create_ba_plot <- function(data, x_var, y_var, title) {
    BA_obj <- agree_nest(data = data, x = x_var, y = y_var, id = "ID", delta = 5, agree.level = 0.95)
    
    gg <- plot(BA_obj) +
      labs(
        y = "Difference of Predicted and Tested 1RM [kg]",
        x = "Mean of Tested and Predicted 1RM [kg]",
        color = "Legend"
      ) +
      geom_point(size = 3, alpha = 0.75, color = "grey10") +
      mytheme +
      scale_x_continuous(limits = c(50, 170)) +
      scale_y_continuous(limits = c(-40, 40))
    
    return(ggplotly(gg))
  }
  
  # Reactive expressions for each plot ====
  output$ba_gen <- renderPlotly({ create_ba_plot(ba_data(), "Tested_1RM", "Gen_Predicted_1RM", "General Model") })
  output$ba_ind <- renderPlotly({ create_ba_plot(ba_data(), "Tested_1RM", "Ind_Predicted_1RM", "Individual Model") })
  output$ba_v1RM <- renderPlotly({ create_ba_plot(ba_data(), "Tested_1RM", "v1RM_Predicted_1RM", "v1RM Model") })
  output$ba_2p_gen <- renderPlotly({ create_ba_plot(ba_data(), "Tested_1RM", "Two_Point_Gen_Predicted_1RM", "General 2-Point Model") })
  output$ba_2p_ind <- renderPlotly({ create_ba_plot(ba_data(), "Tested_1RM", "Two_Point_Ind_Predicted_1RM", "Individual 2-Point Model") })
  output$ba_2p_v1RM <- renderPlotly({ create_ba_plot(ba_data(), "Tested_1RM", "Two_Point_v1RM_Predicted_1RM", "v1RM 2-Point Model") })
  
  # BA Jump Heights
  # Bland-Altman analysis for Agreement
  output$ba_jh <- renderPlotly({
    
    if (input$selected_sex4 != "both") {
      data_jump <- data_jump %>% filter(Sex == input$selected_sex4)
    }
    
    BA <- agree_nest(data = data_jump,
                     x = "Height_Impulse",
                     y = "Height_Flight_Time",
                     id = "ID",
                     delta = 2,
                     agree.level = 0.95)
    
    # Actual B-A plot
    p <- plot(BA) +
      labs(
        y = "Difference of Flight-Time and Impulse Jump Height [cm]",
        x = "Mean of Flight-Time and Impulse Jump Height [cm]",
        color = "Legend"
      ) +
      mytheme +
      geom_point(size = 2, alpha = 0.75, color = "grey10") +
      scale_y_continuous(n.breaks = 10) 
    
    ggplotly(p)
    
  })
  
  # Training Session Table ====
  # Data frame for table (using the same structure as provided in your original code)
  data_table <- data.frame(
    Session = c(1:12),
    `Intensity [% 1RM]` = rep(c(70, 75, 80, 85), each = 3),
    `Sets × Reps` = rep(c("3×8", "4×6", "4×4", "4×3"), each = 3),
    `TS Rest` = rep(c("360", "540", "540", "540"), each = 3),
    `CS Rest` = c(450, 630, 540, 660, 780, 780, 660, 900, 780, 660, 780, 780),
    `CS Configuration` = c("4 + 4", "2 + 2 + 2 + 2", "3 + 2 + 3", "3 + 3", "2 + 2 + 2", 
                           "2 + 2 + 2", "2 + 2", "1 + 1 + 1 + 1", "1 + 1 + 2", "2 + 1", "1 + 1 + 1", "1 + 1 + 1"),
    `Undulating Load` = c("No", "No", "67.5%, 75%, 67.5%", "No", "No", "72.5%, 80%, 72.5%", 
                          "No", "No", "77.5%, 85%, 77.5%", "No", "No", "82.5%, 90%, 82.5%"),
    check.names = FALSE
  )
  
  # Reactive table based on the session filter
  output$session_table <- renderDT({
    # Filter data based on the selected session, showing all if "both" is selected
    filtered_data <- if (input$selected_session == "both") {
      data_table
    } else {
      data_table[data_table$Session == as.numeric(input$selected_session), ]
    }
    
    # Render the table using DT package
    datatable(
      filtered_data,options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        scrollY = '37',
        responsive = TRUE, 
        searching = FALSE,  
        paging = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      class = 'cell-border stripe'
    )
  })
  
  # Velocity Profiles ====
  output$Velocity_profiles <- renderPlotly({
    selected_data <- data_VBT %>%
      filter(Session == input$selected_session) %>%
      group_by(ID, Session, Set) %>%
      mutate(VL = ((max(MPV) - last(MPV)) / first(MPV) * 100)) %>%
      ungroup()
    
    selected_data <- selected_data %>%
      group_by(ID, Session) %>%
      mutate(VL_Overall = ((max(MPV) - last(MPV)) / first(MPV) * 100)) %>%
      ungroup()
    
    if (input$selected_sex2 != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex2)
    }
    
    # Data plot
    selected_data <- selected_data %>%
      group_by(Group, Set, Session, Rep) %>%
      summarize(
        Mean_MPV = mean(MPV, na.rm = TRUE),
        SD_MPV = sd(MPV, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Calculate the yintercept grouped by Group
    yintercepts <- selected_data %>%
      group_by(Group) %>%
      summarize(yintercept = max(Mean_MPV, na.rm = TRUE) * 0.8)
    
    
    selected_data$Set <- as.factor(selected_data$Set)
    
    # Create the plot
    plot <- ggplot(selected_data, aes(x = Rep, y = Mean_MPV, color = Set)) +
      geom_point(size = 3) +
      geom_line() +
      geom_errorbar(aes(ymin = Mean_MPV - SD_MPV,
                        ymax = Mean_MPV + SD_MPV),
                    width = 0.5) +
      geom_hline(data = yintercepts, aes(yintercept = yintercept), 
                 linetype = "dashed", linewidth = 0.5) +  
      scale_color_viridis(discrete = TRUE, option = "C", direction = -1, begin = 0, end = 0.8) +
      facet_grid(scales = "fixed", cols = vars(Group)) +
      scale_x_continuous(name = "Repetition", breaks = seq(2, 24, by = 2)) +
      scale_y_continuous(name = "MPV [m/s]", breaks = seq(0, 1.5, by = 0.1)) +
      mytheme
    
    # Convert ggplot to plotly for interactivity
    plotly_plot <- ggplotly(plot) %>%
      layout(legend = list(
        orientation = "h",  
        x = 0.5,           
        y = -0.2,          
        xanchor = "center",
        yanchor = "top"
      ))
    
    plotly_plot
  })
  
  # Velocity Boxplots ====
  output$Velocity_boxplots <- renderPlotly({
    selected_data <- data_VBT %>%
      filter(Session == input$selected_session) %>%
      group_by(ID, Session, Set) %>%
      mutate(VL = ((max(MPV) - last(MPV)) / first(MPV) * 100)) %>%
      ungroup()
    
    selected_data <- selected_data %>%
      group_by(ID, Session) %>%
      mutate(VL_Overall = ((max(MPV) - last(MPV)) / first(MPV) * 100)) %>%
      ungroup()
    
    if (input$selected_sex2 != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex2)
    }
    
    # Adjust the order of Group levels
    selected_data <- selected_data %>%
      mutate(Group = factor(Group, levels = c("CS", "TS"))) 
    
    plot <- ggplot(selected_data, aes(x = Group, y = MPV)) +
      geom_boxplot(aes(fill = Group), outlier.shape = 8) +
      scale_fill_discrete("aas") +
      stat_summary(fun = mean, geom = "point", shape = 22, size = 4, fill = "black") +
      geom_jitter(alpha = 0.8, size = 0.4) +
      ylab("MPV [m/s]") +
      mytheme +
      theme(legend.position = "none")
    
    # Display plot
    ggplotly(plot)
  })
  
  # Velocity table ====
  output$Velocity_table <- renderDT({
    selected_data <- data_VBT %>%
      filter(Session == input$selected_session) %>%
      group_by(ID, Session, Set) %>%
      mutate(VL = ((max(MPV) - last(MPV)) / max(MPV) * 100)) %>%
      ungroup()
    
    if (input$selected_sex2 != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex2)
    }
    
    # Create summary table
    table_data <- selected_data %>%
      group_by(Group) %>%
      reframe(
        `Total Repetitions` = n(),
        `Mean Load [kg]` = mean(Load, na.rm = TRUE),
        `SD Load [kg]` = sd(Load, na.rm = TRUE),
        `Mean MPV [m/s]` = mean(MPV, na.rm = TRUE),
        `SD MPV [m/s]` = sd(MPV, na.rm = TRUE),
        `Mean VL [%]` = mean(VL, na.rm = TRUE),
        `SD VL [%]` = sd(VL, na.rm = TRUE)
      ) %>%
      # Round numeric data to 2 decimal places using the updated syntax
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      # Adjust column names and format as strings with mean (SD)
      mutate(
        `Load [kg]` = paste0(`Mean Load [kg]`, " (", `SD Load [kg]`, ")"),
        `MPV [m/s]` = paste0(`Mean MPV [m/s]`, " (", `SD MPV [m/s]`, ")"),
        `VL [%]` = paste0(`Mean VL [%]`, " (", `SD VL [%]`, ")")
      ) %>%
      # Convert all columns to character type for pivoting
      mutate(across(everything(), as.character)) %>%
      # Select relevant columns
      select(
        Group,
        `Total Repetitions`,
        `Load [kg]`,
        `MPV [m/s]`,
        `VL [%]`
      )
    
    # Create the datatable output
    datatable(
      table_data,
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = '115',
        responsive = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      class = 'cell-border stripe'
    )
  })
  
  # MPV Plot ====
  output$plot_MPV <- renderPlotly({
    
    if (input$selected_sex2 != "both") {
      data_acute <- data_acute %>% filter(Sex == input$selected_sex2)
    }
    
    A <- ggplot(data_acute, aes(x = Group, y = MPV)) +
      geom_boxplot(aes(fill = Group), outlier.shape = 19) +
      scale_fill_discrete() +
      stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +
      geom_jitter(alpha = 0.8, size = 0.4) +
      ylab("Mean of MPV [m/s]") +
      facet_grid(scales = "fixed", cols = vars(Session)) +
      mytheme
    
    # Convert ggplot to plotly for interactivity
    plotly_plot <- ggplotly(A) %>%
      layout(legend = list(
        orientation = "h",  
        x = 0.5,           
        y = -0.2,          
        xanchor = "center",
        yanchor = "top"
      ))
    
    plotly_plot
  })
  
  # Velocity Loss Plot ====
  output$plot_VL <- renderPlotly({
    
    if (input$selected_sex2 != "both") {
      data_acute <- data_acute %>% filter(Sex == input$selected_sex2)
    }
    
    B <- ggplot(data_acute, aes(x = Group, y = VL)) +
      geom_boxplot(aes(fill = Group), outlier.shape = 19) +
      scale_fill_discrete() +
      stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +
      geom_jitter(alpha = 0.8, size = 0.4) +
      ylab("Mean of VL [%]") +
      facet_grid(scales = "fixed", cols = vars(Session)) +
      mytheme
    
    # Convert ggplot to plotly for interactivity
    plotly_plot <- ggplotly(B) %>%
      layout(legend = list(
        orientation = "h",  
        x = 0.5,           
        y = -0.2,          
        xanchor = "center",
        yanchor = "top"
      ))
    
    plotly_plot
  })
  
  # CMJ Difference Plot ====
  output$plot_CMJ <- renderPlotly({
    
    if (input$selected_sex2 != "both") {
      data_acute <- data_acute %>% filter(Sex == input$selected_sex2)
    }
    
    C <- ggplot(data_acute, aes(x = Group, y = CMJ_dif)) +
      geom_boxplot(aes(fill = Group), outlier.shape = 19) +
      scale_fill_discrete() +
      stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +
      geom_jitter(alpha = 0.8, size = 0.4) +
      ylab("CMJ Difference [%]") +
      facet_grid(scales = "fixed", cols = vars(Session)) +
      mytheme
    
    # Convert ggplot to plotly for interactivity
    plotly_plot <- ggplotly(C) %>%
      layout(legend = list(
        orientation = "h",  
        x = 0.5,           
        y = -0.2,          
        xanchor = "center",
        yanchor = "top"
      ))
    
    plotly_plot
  })
  
  # RSI Mod Difference Plot ====
  output$plot_RSI <- renderPlotly({
    
    if (input$selected_sex2 != "both") {
      data_acute <- data_acute %>% filter(Sex == input$selected_sex2)
    }
    
    D <- ggplot(data_acute, aes(x = Group, y = RSI_mod_dif)) +
      geom_boxplot(aes(fill = Group), outlier.shape = 19) +
      scale_fill_discrete() +
      stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +
      geom_jitter(alpha = 0.8, size = 0.4) +
      ylab("RSI mod. Difference [%]") +
      facet_grid(scales = "fixed", cols = vars(Session)) +
      mytheme
    
    # Convert ggplot to plotly for interactivity
    plotly_plot <- ggplotly(D) %>%
      layout(legend = list(
        orientation = "h",  
        x = 0.5,           
        y = -0.2,          
        xanchor = "center",
        yanchor = "top"
      ))
    
    plotly_plot
  })
  
  # Correlation Matrix ####
  output$cor_matrix <- renderPlot({
    selected_data <- data_fatigue 
    
    if (input$selected_sex2 != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex2)
    }
    
    selected_data <- selected_data %>%
      filter(Session %in% c("2", "5", "8", "11")) %>%
      reframe(
        `ID` = ID,
        `Session` = Session,
        `CMJ Difference` = - ((CMJ_pre_mean - CMJ_post_mean) / CMJ_pre_mean) * 100,
        `RSI mod. Difference` = - ((RSI_mod_pre_mean - RSI_mod_post_mean) / RSI_mod_pre_mean) * 100,
        `v70 Difference` = - ((v70_pre_mean - v70_post_mean) / v70_pre_mean) * 100, 
        `Lactate Difference` =  - (Lactate_pre - Lactate_post), 
        `Muscular Stress pre` = SRSS_pre_5, 
        `Muscular Stress post 0` = SRSS_post0_5, 
        `Muscular Stress post 24` = SRSS_post24_5, 
        `Muscular Stress post 48` = SRSS_post48_5, 
        `Overall Stress pre` = SRSS_pre_8, 
        `Overall Stress post 0` = SRSS_post0_8, 
        `Overall Stress post 24` = SRSS_post24_8, 
        `Overall Stress post 48` = SRSS_post48_8, 
        `RPE` = RPE_mean,
        `DOMS post 24` = DOMS_post24, 
        `DOMS post 48` = DOMS_post48
      )
    
    data_VBT <- data_VBT
    
    if (input$selected_sex2 != "both") {
      data_VBT <- data_VBT %>% filter(Sex == input$selected_sex2)
    }
    
    data_VBT <- data_VBT %>%
      group_by(ID, Session, Set) %>%
      mutate(VL = ((max(MPV) - last(MPV)) / first(MPV) * 100)) %>%
      ungroup()
    
    data_VBT <- data_VBT %>%
      group_by(ID, Session) %>%
      mutate(VL_Overall = ((max(MPV) - last(MPV)) / first(MPV) * 100)) %>%
      ungroup()
    
    data_VBT <- data_VBT %>%
      filter(Session %in% c("2", "5", "8", "11")) %>%
      group_by(ID, Session) %>%
      reframe(
        `ID` = first(ID),
        `Session` = first(Session),
        `VL` = mean(VL)
      )
    
    
    selected_data <- merge(selected_data, data_VBT, by = c("ID", "Session"))
    
    # Select relevant variables 
    parameters <- select(selected_data, starts_with(c("VL","CMJ", "RSI", "v70","Lactate", "Muscular", "Overall", "RPE", "DOMS"))) 
    
    
    # Calculate the correlation matrix
    corr_matrix <- corr_coef(parameters, method = "spearman", use = "pairwise.complete.obs")
    
    
    # plot correlation matrix
    plot(corr_matrix, legend.title = "Spearman's\nCorrelation", reorder = FALSE, 
         col.low = "red", col.mid = "white", col.high = "blue")
  })
  
  # Training Adaptation 1RM ====
  output$boxplot_with_spaghetti <- renderPlotly({
    selected_data <- data_test %>%
      mutate(Time = factor(Time, levels = c("pre", "post")))
    
    if (input$selected_sex3 != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex3)
    }
    
    # 1 RM Graph
    plot <- ggplot(selected_data, aes(x = Time, y = `1RM`, fill = Group)) +
      geom_boxplot(data = selected_data %>%
                     filter(Time == 'pre'),
                   position = position_nudge(-0.3),
                   width = 0.3,
                   lwd = 1) +
      geom_boxplot(data = selected_data %>%
                     filter(Time == 'post'),
                   position = position_nudge(0.3),
                   width = 0.3,
                   lwd = 1)+
      geom_point(data = selected_data %>%
                   filter(Time == 'pre'),
                 position = position_nudge(0.3),
                 size = 3) +
      geom_point(data = selected_data %>%
                   filter(Time == 'post'),
                 position = position_nudge(-0.3),
                 size = 3) +
      geom_line(aes(group = ID),
                position = position_nudge(x = c(0.3, -0.3)),
                lwd = 0.6,
                color = "grey")+
      facet_grid(cols = vars(Group)) +
      ylab("1RM [kg]") +
      mytheme
    
    ggplotly(plot)
    
  })
  
  # Render Velocity-Load Plot ====
  output$plot_VL_profiles <- renderPlotly({
    # Fit mixed-effects model
    lmm <- lmer(MPV ~ Load * Time * Sex * Group + (1 + Load | ID), data = data_1RM)
    data_1RM$predicted_MPV <- predict(lmm, newdata = data_1RM, re.form = NA)
    
    # Function to filter data
    filtered_data <- reactive({
      selected_data <- data_1RM
      if (input$selected_sex3 != "both") {
        selected_data <- selected_data %>% filter(Sex == input$selected_sex3)
      }
      return(selected_data)
    })
    
    data_filtered <- filtered_data()
    
    models_filtered <- data_filtered %>%
      group_by(Sex, Group, Time) %>%
      do({
        model <- lm(predicted_MPV ~ Load, data = .)   
        coefs <- coef(model)
        equation <- paste0("y = ", sprintf("%.4f", coefs[2]), " x + ", sprintf("%.4f", coefs[1]))
        tibble(model = list(model), equation = equation)
      }) %>%
      ungroup()
    
    p <- ggplot(data_filtered, aes(x = Load, y = MPV, color = Time)) +
      geom_point(alpha = 0.3, size = 2) +
      geom_line(aes(y = predicted_MPV), linewidth = 1.5) +
      geom_smooth(aes(group = interaction(ID, Time)), method = "lm", 
                  se = FALSE, linetype = "dashed", linewidth = 0.5) +
      facet_grid(Sex ~ Group) +
      labs(x = "Load [kg]", y = "Mean Propulsive Velocity [m/s]") +
      scale_color_manual(values = c("pre" = "#1f77b4", "post" = "#ff7f0e")) +
      geom_text(data = models_filtered %>% filter(Time == "pre"),
                aes(x = 80, y = 1.25, label = equation), 
                color = "#1f77b4", size = 3, fontface = "bold", hjust = 0) + 
      geom_text(data = models_filtered %>% filter(Time == "post"),
                aes(x = 80, y = 1.15, label = equation), 
                color = "#ff7f0e", size = 3, fontface = "bold", hjust = 0) +
      theme(
        plot.title = element_text(size = 14, color = "black", face = "plain", hjust = 0.5),
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.line = element_line(linewidth = 0.9),
        legend.title = element_text(size = 12, face = "bold", color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        legend.key = element_rect(color = FALSE, fill = "grey"))
    
    plotly_plot <- ggplotly(p) %>%
      layout(legend = list(
        orientation = "h",  
        x = 0.5,           
        y = -0.2,          
        xanchor = "center",
        yanchor = "top"
      ))
    
    plotly_plot
  })
  
  # Adaptation Table ====
  output$Adaptation_table <- renderDT({
    data_test <- data_test %>% select(-v70_mean)
    data_test <- data_test %>%
      left_join(
        data_performance %>%
          select(ID, Time, v70_mean),
        by = c("ID", "Time"))
    
    selected_data <- data_test
    
    if (input$selected_sex3 != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex3)
    }
    
    # Create summary table
    table_data <- selected_data %>%
      group_by(Group, Time) %>%
      mutate(Time = factor(Time, levels = c("pre","post"))) %>%
      reframe(
        `N` = n(),
        `Mean 1RM` = mean(`1RM`, na.rm = TRUE),
        `SD 1RM` = sd(`1RM`, na.rm = TRUE),
        `Mean CMJ` = mean(CMJ_mean, na.rm = TRUE),
        `SD CMJ` = sd(CMJ_mean, na.rm = TRUE),
        `Mean CMJ Force` = mean(CMJ_Force_mean, na.rm = TRUE),
        `SD CMJ Force` = sd(CMJ_Force_mean, na.rm = TRUE),
        `Mean v70` = mean(v70_mean, na.rm = TRUE),
        `SD v70` = sd(v70_mean, na.rm = TRUE),
        `Mean Isometric Force` = mean(Isometric_force_mean, na.rm = TRUE),
        `SD Isometric Force` = sd(Isometric_force_mean, na.rm = TRUE),
        `Mean max reps` = mean(max_reps, na.rm = TRUE),
        `SD max reps` = sd(max_reps, na.rm = TRUE)
      ) %>%
      # Round
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      # Variable names
      mutate(
        `1RM [kg]` = paste0(`Mean 1RM`, " (", `SD 1RM`, ")"),
        `CMJ [cm]` = paste0(`Mean CMJ`, " (", `SD CMJ`, ")"),
        `CMJ Force [N/BW]` = paste0(`Mean CMJ Force`, " (", `SD CMJ Force`, ")"),
        `v70 [m/s]` = paste0(`Mean v70`, " (", `SD v70`, ")"),
        `Isometric Force [N/BW]` = paste0(`Mean Isometric Force`, " (", `SD Isometric Force`, ")"),
        `Max Reps` = paste0(`Mean max reps`, " (", `SD max reps`, ")")
      ) %>%
      # Convert all columns to character type for pivoting
      mutate(across(everything(), as.character)) %>%
      # choose relevant variables
      select(
        Group,
        Time,
        N,
        `1RM [kg]`,
        `CMJ [cm]`,
        `CMJ Force [N/BW]`,
        `v70 [m/s]`,
        `Isometric Force [N/BW]`,
        `Max Reps`
      ) %>%
      # Reshape data: pivot longer then wider
      pivot_longer(cols = -c(Group, Time), names_to = "Measure", values_to = "Value") %>%
      pivot_wider(names_from = c(Group, Time), values_from = Value, names_sep = " ") 
    
    # Create the datatable output
    datatable(
      table_data,
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = '330',
        responsive = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      class = 'cell-border stripe'
    )
  })
  
  # Transfer coefficient Table ####
  output$Adaptation_difference_table <- renderDT({
    data_test <- data_test %>% select(-v70_mean)
    data_test <- data_test %>%
      left_join(
        data_performance %>%
          select(ID, Time, v70_mean),
        by = c("ID", "Time"))
    
    selected_data <- data_test
    
    if (input$selected_sex3 != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex3)
    }
    
    # Pre data
    pre_data <- selected_data %>%
      filter(Time == "pre") %>%
      group_by(Group) %>%
      reframe(
        Mean_1RM_Pre = mean(`1RM`),
        SD_1RM_Pre = sd(`1RM`),
        Mean_CMJ_Pre = mean(CMJ_mean),
        SD_CMJ_Pre = sd(CMJ_mean),
        Mean_v70_Pre = mean(v70_mean),
        SD_v70_Pre = sd(v70_mean),
        Mean_Isometric_Force_Pre = mean(Isometric_force_mean),
        SD_Isometric_Force_Pre = sd(Isometric_force_mean),
        Mean_Max_Reps_Pre = mean(max_reps),
        SD_Max_Reps_Pre = sd(max_reps)
      )
    
    # Post data
    post_data <- selected_data %>%
      filter(Time == "post") %>%
      group_by(Group) %>%
      reframe(
        Mean_1RM_Post = mean(`1RM`),
        Mean_CMJ_Post = mean(CMJ_mean),
        Mean_v70_Post = mean(v70_mean),
        Mean_Isometric_Force_Post = mean(Isometric_force_mean),
        Mean_Max_Reps_Post = mean(max_reps)
      )
    
    # Mean improvements
    improvement <- post_data %>%
      left_join(pre_data, by = "Group") %>%
      mutate(
        Average_Improvement_1RM = Mean_1RM_Post - Mean_1RM_Pre,
        Average_Improvement_CMJ = Mean_CMJ_Post - Mean_CMJ_Pre,
        Average_Improvement_v70 = Mean_v70_Post - Mean_v70_Pre,
        Average_Improvement_Isometric_Force = Mean_Isometric_Force_Post - Mean_Isometric_Force_Pre,
        Average_Improvement_Max_Reps = Mean_Max_Reps_Post - Mean_Max_Reps_Pre
      )
    
    # Standardized improvements
    standardized_improvement <- improvement %>%
      mutate(
        Standardized_Improvement_1RM = Average_Improvement_1RM / SD_1RM_Pre,
        Standardized_Improvement_CMJ = Average_Improvement_CMJ / SD_CMJ_Pre,
        Standardized_Improvement_v70 = Average_Improvement_v70 / SD_v70_Pre,
        Standardized_Improvement_Isometric_Force = Average_Improvement_Isometric_Force / SD_Isometric_Force_Pre,
        Standardized_Improvement_Max_Reps = Average_Improvement_Max_Reps / SD_Max_Reps_Pre
      )
    
    # Rename columns for final output with spaces instead of underscores
    table_data <- standardized_improvement %>%
      select(
        `Group` = Group,
        `Average Improvement 1RM [kg]` = Average_Improvement_1RM,
        `Standardized Improvement 1RM [SD]` = Standardized_Improvement_1RM,
        `Average Improvement CMJ [cm]` = Average_Improvement_CMJ,
        `Standardized Improvement CMJ [SD]` = Standardized_Improvement_CMJ,
        `Average Improvement v70 [m/s]` = Average_Improvement_v70,
        `Standardized Improvement v70 [SD]` = Standardized_Improvement_v70,
        `Average Improvement Isometric Force [N]` = Average_Improvement_Isometric_Force,
        `Standardized Improvement Isometric Force [SD]` = Standardized_Improvement_Isometric_Force,
        `Average Improvement Max Reps` = Average_Improvement_Max_Reps,
        `Standardized Improvement Max Reps [SD]` = Standardized_Improvement_Max_Reps
      ) %>%
      # Round
      mutate(across(where(is.numeric), ~ round(.x, 2)))%>%
      # Reshape data: pivot longer then wider
      pivot_longer(cols = -Group, names_to = "Measure", values_to = "Value") %>%
      pivot_wider(names_from = Group, values_from = Value, names_sep = " ") 
    
    
    # Create the datatable output
    datatable(
      table_data,
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = '330',
        responsive = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      class = 'cell-border stripe'
    )
  })
  
  # CMJ Development ====
  output$CMJ_development <- renderPlotly({
    selected_data <- data_performance 
    
    if (input$selected_sex3 != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex3)
    }
    
    selected_data <- selected_data %>%
      group_by(Time, Group) %>%
      reframe(
        CMJ_mean,
        Mean_CMJ = mean(CMJ_mean, na.rm = TRUE),
        n = n(),
        CI_CMJ = 1.96 * (sd(CMJ_mean, na.rm = TRUE) / sqrt(n))
      ) %>%
      mutate(Time = factor(Time, levels = c("pre", "1", "2", "3", "4", "5", "6",
                                            "7", "8", "9", "10", "11", "12", "post")),
             Time_nudge = as.numeric(Time) + ifelse(Group == "CS", -0.1, 0.1))
    
    # graph
    plot <- ggplot(selected_data, aes(x = Time_nudge, y = Mean_CMJ, color = Group)) +
      geom_point(size = 3) +
      geom_line(aes(group = Group)) +
      geom_errorbar(aes(ymin = Mean_CMJ - CI_CMJ,
                        ymax = Mean_CMJ + CI_CMJ),
                    width = 0.2) +
      geom_jitter(aes(x = Time_nudge, y = CMJ_mean)) +
      scale_x_continuous(breaks = as.numeric(1:length(levels(selected_data$Time))),
                         labels = levels(selected_data$Time)) +
      xlab("Time") +
      scale_y_continuous(name = "CMJ Height [cm]", breaks = seq(20, 60, by = 5)) +
      theme_classic() +
      theme(
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.line = element_line(linewidth = 0.9),
        legend.position = "bottom"  
      )
    
    # Output as plotly
    plotly_plot <- ggplotly(plot) %>%
      layout(legend = list(
        orientation = "h",  
        x = 0.5,           
        y = -0.2,          
        xanchor = "center",
        yanchor = "top"
      ))
    
    plotly_plot
    
  })
  
  # v70 Development ====
  output$v70_development <- renderPlotly({
    selected_data <- data_performance 
    
    if (input$selected_sex3 != "both") {
      selected_data <- selected_data %>% filter(Sex == input$selected_sex3)
    }
    
    selected_data <- selected_data %>%
      group_by(Time, Group) %>%
      reframe(
        v70_mean,
        Mean_v70 = mean(v70_mean, na.rm = TRUE),
        n = n(),
        CI_v70 = 1.96 * (sd(v70_mean, na.rm = TRUE) / sqrt(n))
      ) %>%
      mutate(Time = factor(Time, levels = c("pre", "1", "2", "3", "4", "5", "6",
                                            "7", "8", "9", "10", "11", "12", "post")),
             Time_nudge = as.numeric(Time) + ifelse(Group == "CS", -0.1, 0.1))
    
    # graph
    plot <- ggplot(selected_data, aes(x = Time_nudge, y = Mean_v70, color = Group)) +
      geom_point(size = 3) +
      geom_line(aes(group = Group)) +
      geom_errorbar(aes(ymin = Mean_v70 - CI_v70,
                        ymax = Mean_v70 + CI_v70),
                    width = 0.2) +
      geom_jitter(aes(x = Time_nudge, y = v70_mean)) +
      scale_x_continuous(breaks = as.numeric(1:length(levels(selected_data$Time))),
                         labels = levels(selected_data$Time)) +
      xlab("Time") +
      scale_y_continuous(name = "MPV [m/s]", breaks = seq(0, 1, by = 0.1)) +
      theme_classic() +
      theme(
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.line = element_line(linewidth = 0.9),
        legend.position = "bottom"  
      )
    
    # Output as plotly
    plotly_plot <- ggplotly(plot) %>%
      layout(legend = list(
        orientation = "h",  
        x = 0.5,           
        y = -0.2,          
        xanchor = "center",
        yanchor = "top"
      ))
    
    plotly_plot
    
  })
  
  # Render Raincloud Plot for 1RM Distribution ====
  output$plot_1RM_distribution <- renderPlot({
    data_pre <- data_test %>% filter(Time == "pre")
    
    plot_raincloud(data_pre, value = "rel_1RM", value_label = "rel. 1RM [kg / BW]", groups = "Sex",
                   plot_control(group_colors = c("deeppink", "blue3"))) +
      geom_vline(aes(xintercept = 1), linewidth = 0.75, linetype = "dotted") +
      geom_vline(aes(xintercept = 1.25), linewidth = 0.75, linetype = "dotted") +
      scale_x_continuous(breaks = c(0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5)) +
      theme(
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.line = element_line(linewidth = 0.9))
    
  })
  
  # Render Characteristics Table ====
  output$table_characteristics <- render_gt({
    characteristics <- data_test %>%
      filter(Time == "pre") %>%
      group_by(Sex) %>%
      summarise(
        across(
          c(Age, Body_weight, BMI, Body_fat, FFM, `1RM`, rel_1RM),
          ~ sprintf("%.2f ± %.2f", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE)),
          .names = "{.col}"
        ),
        .groups = "drop"
      )
    
    characteristics %>%
      gt() %>%
      cols_label(
        Age = "Age [years]",
        Body_weight = "Body Weight [kg]",
        BMI = html("BMI [kg/m<sup>2</sup>]"),
        Body_fat = "Body Fat [%]",
        FFM = "FFM [kg]",
        `1RM` = "1RM [kg]",
        rel_1RM = "rel. 1RM [kg/BW]",
        Sex = "Sex"
      ) %>%
      cols_align(align = "center") %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      )
  })
}


# Create Shiny App
shinyApp(ui = ui, server = server)
