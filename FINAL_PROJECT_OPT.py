import math                                    
import gurobipy as gp                          
from gurobipy import GRB                      


#teams and divisions
teams = [                                      
    'Baltimore Orioles', 'Boston Red Sox', 'New York Yankees', 'Tampa Bay Rays',
    'Toronto Blue Jays', 'Chicago White Sox', 'Cleveland Guardians',
    'Detroit Tigers', 'Kansas City Royals', 'Minnesota Twins',
    'Houston Astros', 'Los Angeles Angels', 'Seattle Mariners',
    'Texas Rangers', 'Oakland Athletics', 'Atlanta Braves', 'Miami Marlins',
    'New York Mets', 'Philadelphia Phillies', 'Washington Nationals',
    'Chicago Cubs', 'Cincinnati Reds', 'Milwaukee Brewers',
    'Pittsburgh Pirates', 'St. Louis Cardinals', 'Arizona Diamondbacks',
    'Colorado Rockies', 'Los Angeles Dodgers', 'San Diego Padres',
    'San Francisco Giants'
]

#defining each division as a set 
al_east    = {'Baltimore Orioles','Boston Red Sox','New York Yankees',
              'Tampa Bay Rays','Toronto Blue Jays'}
al_central = {'Chicago White Sox','Cleveland Guardians','Detroit Tigers',
              'Kansas City Royals','Minnesota Twins'}
al_west    = {'Houston Astros','Los Angeles Angels','Seattle Mariners',
              'Texas Rangers','Oakland Athletics'}
nl_east    = {'Atlanta Braves','Miami Marlins','New York Mets',
              'Philadelphia Phillies','Washington Nationals'}
nl_central = {'Chicago Cubs','Cincinnati Reds','Milwaukee Brewers',
              'Pittsburgh Pirates','St. Louis Cardinals'}
nl_west    = {'Arizona Diamondbacks','Colorado Rockies','Los Angeles Dodgers',
              'San Diego Padres','San Francisco Giants'}

divisions = [                               
    al_east, al_central, al_west,
    nl_east, nl_central, nl_west
]


#season timing parameters
#since the schedule is so big we did 162 / 3 so then for each game we just do times 3 to make a 3 game serie
games     = 54                             
num_weeks = 9                              
num_days  = num_weeks * 8                  
days      = list(range(num_days))         


#Ballpark Coordinates & Haversine Function
#bind tuples of floats to stadium abbreviations
#https://docs.google.com/spreadsheets/d/1p0R5qqR7XjoRG2mR5E1D_trlygHSqMOUdMgMpzq0gjU/htmlview
ARI = (33.4484, -111.9826); ATL = (33.9064,  -84.3717)
BAL = (39.2904,  -76.6122); BOS = (42.3601,  -71.0589)
CHC = (41.9484,  -87.6553); CHW = (41.8320,  -87.6557)
CIN = (39.1021,  -84.5163); CLE = (41.4993,  -81.6944)
COL = (39.7392, -105.0118); DET = (42.3314,  -83.0467)
HOU = (29.7604,  -95.3698); KAN = (39.0997,  -94.5783)
LAA = (33.8352, -117.9776); LAD = (34.0522, -118.2437)
MIA = (25.7617,  -80.1958); MIL = (43.0308,  -87.9056)
MIN = (44.9743,  -93.2349); NYM = (40.7503,  -73.8486)
NYY = (40.8237,  -73.9356); OAK = (37.8198, -122.2711)
PHI = (39.9526,  -75.1639); PIT = (40.4406,  -80.0006)
SD  = (32.7157, -117.1611); SF  = (37.7749, -122.4194)
SEA = (47.6062, -122.3321); STL = (38.6277,  -90.1946)
TB  = (27.7557,  -82.4604); TEX = (32.7170,  -96.7509)
TOR = (43.6416,  -79.3894); WAS = (38.8730,  -77.0074)

#building dictionary mapping full team names with latitude and longitude tuples
coords = {
    'Arizona Diamondbacks': ARI, 'Atlanta Braves': ATL,
    'Baltimore Orioles':    BAL, 'Boston Red Sox': BOS,
    'Chicago Cubs':         CHC, 'Chicago White Sox': CHW,
    'Cincinnati Reds':      CIN, 'Cleveland Guardians': CLE,
    'Colorado Rockies':     COL, 'Detroit Tigers': DET,
    'Houston Astros':       HOU, 'Kansas City Royals': KAN,
    'Los Angeles Angels':   LAA, 'Los Angeles Dodgers': LAD,
    'Miami Marlins':        MIA, 'Milwaukee Brewers': MIL,
    'Minnesota Twins':      MIN, 'New York Mets': NYM,
    'New York Yankees':     NYY, 'Oakland Athletics': OAK,
    'Philadelphia Phillies':PHI, 'Pittsburgh Pirates': PIT,
    'San Diego Padres':     SD,  'San Francisco Giants': SF,
    'Seattle Mariners':     SEA, 'St. Louis Cardinals': STL,
    'Tampa Bay Rays':       TB,  'Texas Rangers': TEX,
    'Toronto Blue Jays':    TOR, 'Washington Nationals': WAS
}


#https://stackoverflow.com/questions/4913349/haversine-formula-in-python-bearing-and-distance-between-two-gps-points
#haversine formula that calculates the longitude and latitude distance
def haversine(c1, c2):
    lat1, lon1 = math.radians(c1[0]), math.radians(c1[1])   
    lat2, lon2 = math.radians(c2[0]), math.radians(c2[1])   
    dlat, dlon = lat2 - lat1, lon2 - lon1                  
    a = (math.sin(dlat/2)**2 +
         math.cos(lat1)*math.cos(lat2)*math.sin(dlon/2)**2)
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))      
    return 3958.8 * c                                       

# Build dist[(home, away)] by calling haversine for every ordered team pair
dist = {
    (i, j): haversine(coords[i], coords[j])
    for i in teams for j in teams if i != j
}


#model & variables
m = gp.Model('MLB_Schedule')                            

#x[h,a,d] = binary var: 1 if h hosts a on day d
x = m.addVars(teams, teams, days, vtype=GRB.BINARY, name="game")

#y[t,o] = binary var: 1 if team t ever plays opponent o
y = m.addVars(teams, teams, vtype=GRB.BINARY, name="opp_indicator")


#constraints
#exact games total games per team (home + away)
for t in teams:
    m.addConstr(
        gp.quicksum(x[t,a,d] for a in teams if a != t for d in days)
      + gp.quicksum(x[h,t,d] for h in teams if h != t for d in days)
      == games,
        name=f"games_per_team_{t}"
    )

#at most one game per team each day
for t in teams:
    for d in days:
        m.addConstr(
            gp.quicksum(x[t,a,d] for a in teams if a != t)
          + gp.quicksum(x[h,t,d] for h in teams if h != t)
          <= 1,
            name=f"one_game_per_day_{t}_{d}"
        )

#no more than 6 games per week in a 7 day window
for t in teams:
    for w in range(num_weeks):
        week_days = range(w*7, min((w+1)*7, num_days))
        m.addConstr(
            gp.quicksum(x[t,a,d] for a in teams if a != t for d in week_days)
          + gp.quicksum(x[h,t,d] for h in teams if h != t for d in week_days)
          <= 6,
            name=f"week_limit_{t}_w{w}"
        )

#each team must play at least 20 opponents
for t in teams:
    #y[t,o] to actual games: if any x[t,o,d] + x[o,t,d]>0 then y[t,o] must be 1
    for o in teams:
        if t == o: continue
        m.addConstr(
            y[t,o] >= gp.quicksum(x[t,o,d] + x[o,t,d] for d in days) / games,
            name=f"link_y_{t}_{o}"
        )
    #sum of distinct opponents y[t,o] ≥ 20
    m.addConstr(
        gp.quicksum(y[t,o] for o in teams if o != t) >= 20,
        name=f"min_opponents_{t}"
    )

#division rules
#13 games with 7 home for the first team
for div in divisions:
    for i in div:
        for j in div:
            if i >= j:
                continue
            #total games vs. each division rival = 13
            m.addConstr(
                gp.quicksum(x[i,j,d] + x[j,i,d] for d in days) == 13,
                name=f"div_total_{i}_{j}"
            )
            #i hosts 7 of those games
            m.addConstr(
                gp.quicksum(x[i,j,d] for d in days) == 7,
                name=f"div_home_{i}_{j}"
            )
            #j hosts the remaining 6 games
            m.addConstr(
                gp.quicksum(x[j,i,d] for d in days) == 6,
                name=f"div_home_{j}_{i}"
            )


#objective would be to minimize total travel distance across all games
m.setObjective(
    gp.quicksum(dist[(a,h)] * x[h,a,d]
                for h in teams for a in teams if a != h for d in days),
    GRB.MINIMIZE
)

m.optimize()                                           

#https://docs.gurobi.com/projects/optimizer/en/current/reference/python/model.html
#https://support.gurobi.com/hc/en-us/community/posts/24160864478097-Printing-the-values-of-variables-at-optimal-solution
if m.status == GRB.OPTIMAL:                           
    print(f"{'Day':>4}   {'Away':25s} @ {'Home'}")
    print("-" * 60)
    for d in days:
        for h in teams:
            for a in teams:
                if a != h and x[h,a,d].X > 0.5:
                    print(f"{d:>4d}   {a:25s} @ {h}")

#https://www.gurobi.com/documentation/current/refman/py_python_api_overview.html
#https://stackoverflow.com/questions/48038477/cant-figure-out-optimization-function-that-does-what-i-want-in-gurobi
team = 'Texas Rangers'
distance_traveled = sum(
    dist[(team, h)] * x[h, team, d].X    
    for h in teams if h != team
    for d in days
)
print(f"Total distance traveled by {team}: {distance_traveled:.1f} miles")



