def extract_owner:
  .data.owner
  | {
      type: (.type | ascii_upcase),
      login: .login,
      url: .url,
      avatarUrl: .avatarUrl,
    }
;

def extract_project:
  .data.owner.projectV2
  | {
      id: .id,
      number: .number,
      title: .title,
      url: .url,
      shortDescription: .shortDescription,
      readme: .readme,
    }
;

def extract_field:
  . as $field
  | {
      id: $field.id,
      name: $field.name,
      dataType: $field.dataType,
    }
  + (
      if $field.__typename == "ProjectV2IterationField" then
        { configuration: $field.configuration }
      elif $field.__typename == "ProjectV2SingleSelectField" then
        { options: $field.options }
      else
        {}
      end
    )
;

def extract_fields:
  [.[].data.owner.projectV2.fields.nodes[] | extract_field] | flatten
;

. as $data
  | ($data[0] | extract_owner) as $owner
  | ($data[0] | extract_project) as $project
  | ($data | extract_fields) as $fields
  | $project
  | . + {
      owner: $owner,
      fields: $fields
    }
