--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.19
-- Dumped by pg_dump version 9.5.19

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: customers; Type: TABLE; Schema: public; Owner: myuser
--

CREATE TABLE public.customers (
    id integer NOT NULL,
    name text DEFAULT ''::text,
    nr_orders integer DEFAULT 0,
    country text DEFAULT 'England'::text,
    created_at timestamp without time zone DEFAULT now()
);


ALTER TABLE public.customers OWNER TO myuser;

--
-- Name: customers_id_seq; Type: SEQUENCE; Schema: public; Owner: myuser
--

CREATE SEQUENCE public.customers_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.customers_id_seq OWNER TO myuser;

--
-- Name: customers_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: myuser
--

ALTER SEQUENCE public.customers_id_seq OWNED BY public.customers.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: myuser
--

ALTER TABLE ONLY public.customers ALTER COLUMN id SET DEFAULT nextval('public.customers_id_seq'::regclass);


--
-- Data for Name: customers; Type: TABLE DATA; Schema: public; Owner: myuser
--

COPY public.customers (id, name, nr_orders, country, created_at) FROM stdin;
2	Pippi Långstrump	3	Bulgaria	2019-08-19 09:41:30.78888
1	Bilbo Begins	11	Bulgaria	2019-08-19 09:40:31.396807
3	Viktualia Rullgardina	0	Bulgaria	2019-08-19 09:42:52.723223
4	Krusmynta Efraimsdotter	5	Bulgaria	2019-08-19 09:43:04.083209
5	Ana Karenina	0	Russia	2019-08-20 15:41:50.244971
7	Jiji Lolobridgida	0	Italy	2019-08-20 15:42:26.020113
6	Viktor Savashkin	8	Russia	2019-08-20 15:42:07.213557
\.


--
-- Name: customers_id_seq; Type: SEQUENCE SET; Schema: public; Owner: myuser
--

SELECT pg_catalog.setval('public.customers_id_seq', 1, true);


--
-- Name: customers_pkey; Type: CONSTRAINT; Schema: public; Owner: myuser
--

ALTER TABLE ONLY public.customers
    ADD CONSTRAINT customers_pkey PRIMARY KEY (id);


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

