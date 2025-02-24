------------------------------------------------------------------------------
--                       Database interface utilities                       --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
------------------------------------------------------------------------------

with GNATCOLL.SQL; use GNATCOLL.SQL;
package Database_Names is
   pragma Style_Checks (Off);
   TC_Action_Item : aliased constant String := "action_item";
   Ta_Action_Item : constant Cst_String_Access := TC_Action_Item'Access;
   TC_Config : aliased constant String := "config";
   Ta_Config : constant Cst_String_Access := TC_Config'Access;
   TC_Contract : aliased constant String := "contract";
   Ta_Contract : constant Cst_String_Access := TC_Contract'Access;
   TC_Mailing_List : aliased constant String := "mailing_list";
   Ta_Mailing_List : constant Cst_String_Access := TC_Mailing_List'Access;
   TC_Mailing_List_Recipients : aliased constant String := "mailing_list_recipients";
   Ta_Mailing_List_Recipients : constant Cst_String_Access := TC_Mailing_List_Recipients'Access;
   TC_Mailing_List_Subscription_Type : aliased constant String := "mailing_list_subscription_type";
   Ta_Mailing_List_Subscription_Type : constant Cst_String_Access := TC_Mailing_List_Subscription_Type'Access;
   TC_Region : aliased constant String := "region";
   Ta_Region : constant Cst_String_Access := TC_Region'Access;
   TC_Sales_Entity : aliased constant String := "sales_entity";
   Ta_Sales_Entity : constant Cst_String_Access := TC_Sales_Entity'Access;
   TC_Staff : aliased constant String := "staff";
   Ta_Staff : constant Cst_String_Access := TC_Staff'Access;
   TC_Staff_Email : aliased constant String := "staff_email";
   Ta_Staff_Email : constant Cst_String_Access := TC_Staff_Email'Access;
   TC_Subscription : aliased constant String := "subscription";
   Ta_Subscription : constant Cst_String_Access := TC_Subscription'Access;
   TC_Tn_Status : aliased constant String := "tn_status";
   Ta_Tn_Status : constant Cst_String_Access := TC_Tn_Status'Access;
   TC_Tracking_Number : aliased constant String := "tracking_number";
   Ta_Tracking_Number : constant Cst_String_Access := TC_Tracking_Number'Access;
   TC_Wavefront : aliased constant String := "wavefront";
   Ta_Wavefront : constant Cst_String_Access := TC_Wavefront'Access;
   TC_Wavefront_Status : aliased constant String := "wavefront_status";
   Ta_Wavefront_Status : constant Cst_String_Access := TC_Wavefront_Status'Access;
   TC_Wavefront_Tn : aliased constant String := "wavefront_tn";
   Ta_Wavefront_Tn : constant Cst_String_Access := TC_Wavefront_Tn'Access;

   NC_Act_Nb : aliased constant String := """act_Nb""";
   N_Act_Nb : constant Cst_String_Access := NC_act_Nb'Access;
   NC_Active : aliased constant String := "active";
   N_Active : constant Cst_String_Access := NC_active'Access;
   NC_Assignee : aliased constant String := "assignee";
   N_Assignee : constant Cst_String_Access := NC_assignee'Access;
   NC_Comment : aliased constant String := """comment""";
   N_Comment : constant Cst_String_Access := NC_comment'Access;
   NC_Contract_Nb : aliased constant String := "contract_nb";
   N_Contract_Nb : constant Cst_String_Access := NC_contract_nb'Access;
   NC_Contract_Type : aliased constant String := "contract_type";
   N_Contract_Type : constant Cst_String_Access := NC_contract_type'Access;
   NC_Created_By : aliased constant String := "created_by";
   N_Created_By : constant Cst_String_Access := NC_created_by'Access;
   NC_Date_Created : aliased constant String := "date_created";
   N_Date_Created : constant Cst_String_Access := NC_date_created'Access;
   NC_Date_Done : aliased constant String := "date_done";
   N_Date_Done : constant Cst_String_Access := NC_date_done'Access;
   NC_Default_Status : aliased constant String := "default_status";
   N_Default_Status : constant Cst_String_Access := NC_default_status'Access;
   NC_Delivered_By : aliased constant String := "delivered_by";
   N_Delivered_By : constant Cst_String_Access := NC_delivered_by'Access;
   NC_Delivery_Date : aliased constant String := "delivery_date";
   N_Delivery_Date : constant Cst_String_Access := NC_delivery_date'Access;
   NC_Email : aliased constant String := "email";
   N_Email : constant Cst_String_Access := NC_email'Access;
   NC_Email_Address : aliased constant String := "email_address";
   N_Email_Address : constant Cst_String_Access := NC_email_address'Access;
   NC_Id : aliased constant String := "id";
   N_Id : constant Cst_String_Access := NC_id'Access;
   NC_List : aliased constant String := "list";
   N_List : constant Cst_String_Access := NC_list'Access;
   NC_Login : aliased constant String := "login";
   N_Login : constant Cst_String_Access := NC_login'Access;
   NC_Name : aliased constant String := """name""";
   N_Name : constant Cst_String_Access := NC_name'Access;
   NC_Preferred_Email : aliased constant String := "preferred_email";
   N_Preferred_Email : constant Cst_String_Access := NC_preferred_email'Access;
   NC_Priority : aliased constant String := "priority";
   N_Priority : constant Cst_String_Access := NC_priority'Access;
   NC_Region : aliased constant String := "region";
   N_Region : constant Cst_String_Access := NC_region'Access;
   NC_Request_Date : aliased constant String := "request_date";
   N_Request_Date : constant Cst_String_Access := NC_request_date'Access;
   NC_Requested_By : aliased constant String := "requested_by";
   N_Requested_By : constant Cst_String_Access := NC_requested_by'Access;
   NC_Salary : aliased constant String := "salary";
   N_Salary : constant Cst_String_Access := NC_salary'Access;
   NC_Sales_Message : aliased constant String := "sales_message";
   N_Sales_Message : constant Cst_String_Access := NC_sales_message'Access;
   NC_Sales_Rep : aliased constant String := "sales_rep";
   N_Sales_Rep : constant Cst_String_Access := NC_sales_rep'Access;
   NC_Se_Nb : aliased constant String := "se_nb";
   N_Se_Nb : constant Cst_String_Access := NC_se_nb'Access;
   NC_Staff : aliased constant String := "staff";
   N_Staff : constant Cst_String_Access := NC_staff'Access;
   NC_Status : aliased constant String := "status";
   N_Status : constant Cst_String_Access := NC_status'Access;
   NC_Subscription_Nb : aliased constant String := "subscription_nb";
   N_Subscription_Nb : constant Cst_String_Access := NC_subscription_nb'Access;
   NC_Subscription_Type : aliased constant String := "subscription_type";
   N_Subscription_Type : constant Cst_String_Access := NC_subscription_type'Access;
   NC_Tn : aliased constant String := "tn";
   N_Tn : constant Cst_String_Access := NC_tn'Access;
   NC_Wave : aliased constant String := "wave";
   N_Wave : constant Cst_String_Access := NC_wave'Access;
   NC_What_Done : aliased constant String := "what_done";
   N_What_Done : constant Cst_String_Access := NC_what_done'Access;
   NC_Who_Done : aliased constant String := "who_done";
   N_Who_Done : constant Cst_String_Access := NC_who_done'Access;
end Database_Names;
