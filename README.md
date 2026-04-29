# Electricity-Bill-Minimization-Using-Linear-Optimization

**Optimally scheduling energy consumption to reduce electricity costs using Linear Programming (LP) in R.**

### Project Description

In today's world, electricity tariffs often vary significantly throughout the day under **Time-of-Use (TOU)** pricing structures. Peak hours come with higher rates, while off-peak hours offer cheaper electricity. Many household and commercial loads (such as water heaters, washing machines, EV chargers, air conditioners, or other flexible appliances) can be shifted in time without affecting user comfort.

This project addresses the **Electricity Bill Minimization Problem** by formulating it as a **Linear Programming (LP)** optimization model. The goal is to determine the optimal power consumption schedule across different time slots (typically hourly) that **minimizes the total electricity cost** while meeting all practical constraints.

#### Key Objectives
- Minimize total daily electricity cost = Σ (Power consumed in hour t × Electricity price in hour t)
- Satisfy the total daily energy demand required by the user/household
- Respect maximum power limits per time slot (to avoid overloading circuits or exceeding sanctioned load)
- Incorporate user-defined scheduling flexibility and comfort constraints
- Provide a clear comparison between the **baseline (unoptimized)** consumption pattern and the **optimized** schedule

#### Why Linear Programming?
Linear Programming is perfectly suited for this problem because:
- The objective function (cost) and most constraints are linear.
- It guarantees a global optimal solution efficiently.
- The model is easy to understand, modify, and scale.

The solution is implemented entirely in **R**, making it accessible for students, researchers, and data analysts interested in Operations Research, Energy Management, and Optimization.

### Problem Formulation Highlights

- **Decision Variables**: Amount of power (kW) to be consumed or shifted in each time period (e.g., every hour of the day).
- **Objective Function**: Minimize total billing cost based on hourly TOU tariffs.
- **Constraints**:
  - Total energy consumption over 24 hours must equal the required daily demand.
  - Power consumption in any hour cannot exceed the maximum allowable limit.
  - Non-negativity constraints (consumption cannot be negative).
  - Optional: Appliance-specific scheduling windows (e.g., washing machine can only run between 10 PM – 6 AM).

### Technologies & Tools
- **Programming Language**: R
- **Optimization Solver**: `lpSolve` (or `ompr` – depending on your implementation)
- **Data Handling & Visualization**: `dplyr`, `ggplot2`, `readr`
- **Documentation**: LaTeX / PDF report + Presentation slides

### What’s Inside the Repository
- `code.R` → Complete R script with data loading, LP model formulation, solving, and result analysis
- `electricity_bill_dataset.csv` → Sample hourly consumption data and Time-of-Use tariff rates
- Visualizations showing baseline vs. optimized load profile
- Final project report (`report_final.pdf`)
- Presentation slides

### Results & Benefits
Users can expect **noticeable cost savings** (typically 15–35% depending on tariff structure and load flexibility) by intelligently shifting flexible consumption to cheaper hours.

The project clearly demonstrates:
- How much cost can be saved
- How the load curve changes after optimization
- The economic value of demand-side flexibility in smart grids

### Real-World Applications
- Smart home energy management systems
- Industrial load scheduling
- Integration with solar PV and battery storage (future extension)
- Policy analysis for demand response programs

This project serves as an excellent practical example of applying **Operations Research** and **Mathematical Optimization** to solve everyday problems in the energy sector.

**Author**: Ankesh Raj, Md Danish

**Repository**: [anii0021/Electricity-Bill-Minimization-Using-Linear-Optimization](https://github.com/anii0021/Electricity-Bill-Minimization-Using-Linear-Optimization)

