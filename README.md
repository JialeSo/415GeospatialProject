# README: Geospatial Analytics Project - Exploring Traffic and Mobility Patterns in Jakarta

---

## **Project Overview**

This project focuses on analyzing traffic and mobility patterns in Jakarta using geospatial data. The goal is to provide insights into ride-hailing demand, identify high-traffic zones, and understand spatial interaction trends across the city. The project is implemented as a web application with interactive maps and visualizations.

---

## **Deployment**

This project is simple to set up and run locally with minimal prerequisites. It uses modern web technologies and geospatial tools to deliver interactive insights.

### **Prerequisites**

Before you begin, ensure the following are installed on your system:

- [Node.js](https://nodejs.org) (v16 or higher recommended)
- [npm](https://www.npmjs.com/) (included with Node.js)

---

## **Getting Started**
### **Setup and Start the Application**

1. Clone the repository:
   ```bash
   git clone https://github.com/JialeSo/415GeospatialProject
   ```

2. Navigate to the project directory and install dependencies:
   ```bash
   npm i
   ```

3. Start the development server:
   ```bash
   npm run dev
   ```

4. Open your browser and visit:
   ```
   http://localhost:3000
   ```

Your geospatial application is now running locally, ready to showcase Jakarta's traffic and mobility insights.

---

## **Simplified Project Structure**

```
415GeospatialProject/
├── About/
├── ApplicationUserGuide/
├── Geospatial.Rproj
├── Poster/
├── Proposal/
│   └── images/
│       ├── Test.png
│       ├── clipboard-*.png (other relevant images)
├── README.md
├── UserGuide/
├── WebShinyApp/
│   ├── app.R
│   ├── components/
│   │   ├── footer.r
│   │   ├── header.r
│   │   └── sidebar.r
│   ├── datasource/
│   │   ├── jakarta_district.rds
│   │   ├── jakarta_district_centroid.rds
│   │   ├── jakarta_district_population.rds
│   │   ├── jakarta_poi_final.rds
│   ├── tabs/
│   │   ├── datasets.r
│   │   ├── exploratory_data.r
│   │   ├── exploratory_spatial.r
│   │   ├── kernel_density.r
│   │   ├── lisa_analysis.r
│   │   ├── od_analysis.r
│   │   └── project_overview.r
│   └── www/
│       ├── custom.css
│       └── logo.png
├── _quarto.yml
├── images/
├── index.qmd
└── styles.css

```

## **Stopping the Application**

To stop the development server, press `Ctrl + C` in the terminal where it is running.

---

## **Troubleshooting**

If you encounter any issues:

1. Ensure you have the required Node.js and npm versions installed.
2. Run `npm install` again to make sure all dependencies are correctly installed.
3. Check the terminal logs for errors during `npm run dev`.

---

## **Logs**

This application logs server events directly to the terminal where the `npm run dev` command is executed.

---

Enjoy exploring Jakarta's geospatial mobility insights with StreetSurfers!