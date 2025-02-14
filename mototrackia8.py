import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
from datetime import datetime
from PIL import Image
import imageio

# Fonction pour générer des données simulées avec des horodatages à l'heure près pour chaque jour
def generate_data():
    np.random.seed(42)
    date_range = pd.date_range(start="2024-01-01", end="2024-12-31", freq='H')
    
    data = {
        "Date": date_range,
        "Livraisons Effectuées": np.random.randint(800, 1200, len(date_range)),
        "Taux de Respect des Délais": np.random.uniform(85, 98, len(date_range)),
        "Satisfaction Client (%)": np.random.uniform(70, 95, len(date_range)),
        "Nombre dIncidents": np.random.randint(0, 10, len(date_range)),
        "Taux de Retour Produit (%)": np.random.uniform(3, 10, len(date_range)),
        "État des Véhicules (%)": np.random.uniform(80, 99, len(date_range)),
        "Satisfaction des Livreurs (%)": np.random.uniform(65, 90, len(date_range)),
        "Taux d'Accidents (%)": np.random.uniform(0, 5, len(date_range)),
        "Disponibilité des Machines (%)": np.random.uniform(85, 99, len(date_range)),
        "Alertes en Cours": np.random.choice([0, 1], len(date_range), p=[0.95, 0.05])  # 5% de chance d'alerte active
    }
    return pd.DataFrame(data)

# Configuration de la page
st.set_page_config(page_title="Dashboard Intelligente", layout="wide")

# Volets de navigation
st.sidebar.title("Navigation")
page = st.sidebar.radio("Choisir une page", ["Résumé des KPI", "Données et Graphiques"])

# Titre du tableau de bord
st.title("MotoTrack AI - Propulsé par l'Innovation 🚀")
st.markdown("Bienvenue sur le tableau de bord interactif **MotoTrack AI** ! Voici les **données** que nous avons analysées.")
# Affichage d'une image (uniquement si nécessaire)
st.subheader("Image Exemple")
#image = Image.open("D:/chemin/vers/DALL·E 2025-02-14 13.22.19.jpg")  # Mettre ici le bon chemin vers votre image
# Charger l'image .webp
image = imageio.imread("D:/DALL.webp")
st.image(image)


if page == "Résumé des KPI":
    # Section "Résumé des KPI"
    st.markdown("### Résumé des KPI/KQI")
    st.markdown("Les **indicateurs** clés de performance suivants sont suivis quotidiennement pour évaluer la performance du service :")
    st.markdown("- **Livraisons Effectuées** : Nombre total de livraisons réalisées.")
    st.markdown("- **Taux de Respect des Délais** : Pourcentage des livraisons réalisées dans les délais.")
    st.markdown("- **Satisfaction Client** : Indicateur de satisfaction des clients.")
    st.markdown("- **Nombre d'Incidents** : Nombre total d'incidents enregistrés.")
    st.markdown("- **Taux de Retour Produit** : Pourcentage de retours produits.")
    st.markdown("- **État des Véhicules** : Pourcentage d'état optimal des véhicules.")
    st.markdown("- **Satisfaction des Livreurs** : Niveau de satisfaction des livreurs.")
    st.markdown("- **Taux d'Accidents** : Pourcentage d'accidents enregistrés par heure.")
    st.markdown("- **Disponibilité des Machines** : Pourcentage de disponibilité des machines de transport.")
    st.markdown("- **Alertes en Cours** : Indicateur des alertes actives dans le système.")

elif page == "Données et Graphiques":
    # Section "Données et Graphiques"
    # Sélecteur de date
    selected_date = st.date_input("Choisir une date", value=datetime(2024, 1, 1))
    
    # Sélecteur d'heure de début et de fin
    start_hour = st.slider("Heure de début", 0, 23, 0)
    end_hour = st.slider("Heure de fin", 0, 23, 23)

    # Génération des données
    df = generate_data()
    df['Date'] = pd.to_datetime(df['Date'])

    # Filtrage des données pour la date sélectionnée et l'heure
    df_filtered = df[(df['Date'].dt.date == selected_date) &
                     (df['Date'].dt.hour >= start_hour) &
                     (df['Date'].dt.hour <= end_hour)]

    # Affichage des données filtrées dans un tableau
    st.subheader("Tableau des données filtrées")
    st.dataframe(df_filtered)

    # Génération des graphiques dynamiques
    fig1 = px.line(df_filtered, x="Date", y=["Livraisons Effectuées", "Taux de Respect des Délais"],
                   title="📦 Évolution des Livraisons et Délais")
    fig2 = px.line(df_filtered, x="Date", y="Satisfaction Client (%)", title="😊 Satisfaction Client")
    fig3 = px.bar(df_filtered, x="Date", y="Nombre dIncidents", title="🚨 Nombre d'Incidents par Heure", color="Nombre dIncidents")
    fig4 = px.line(df_filtered, x="Date", y="État des Véhicules (%)", title="🏍️ État Global des Véhicules")
    fig5 = px.line(df_filtered, x="Date", y="Satisfaction des Livreurs (%)", title="🚚 Satisfaction des Livreurs")
    fig6 = px.line(df_filtered, x="Date", y="Taux d'Accidents (%)", title="⚠️ Taux d'Accidents")
    fig7 = px.line(df_filtered, x="Date", y="Disponibilité des Machines (%)", title="🔧 Disponibilité des Machines")
    fig8 = px.bar(df_filtered, x="Date", y="Alertes en Cours", title="🔴 Alertes en Cours", color="Alertes en Cours")

    # Affichage des graphiques
    st.plotly_chart(fig1, use_container_width=True)
    st.plotly_chart(fig2, use_container_width=True)
    st.plotly_chart(fig3, use_container_width=True)
    st.plotly_chart(fig4, use_container_width=True)
    st.plotly_chart(fig5, use_container_width=True)
    st.plotly_chart(fig6, use_container_width=True)
    st.plotly_chart(fig7, use_container_width=True)
    st.plotly_chart(fig8, use_container_width=True)

    # Affichage des réponses
    if st.button("Soumettre"):
        st.write(f"Nom: {user_name}")
        st.write(f"Email: {email}")
        st.write(f"Satisfaction: {rating}/5")
        st.write(f"Commentaires: {feedback}")

