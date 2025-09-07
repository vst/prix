def extract_organization_info:
  .data.owner
  | {
      type: (.type | ascii_upcase),
      login: .login,
      url: .url,
      avatarUrl: .avatarUrl,
    }
;

def extract_project_info:
  .data.owner.projectV2
  | {
      number: .number,
      title: .title,
      url: .url,
    }
;

def extract_project_item:
  . | {
        id: .id,
        status: .status.value,
        iteration: (.iteration | if . then (.value | capture("Iteration (?<num>\\d+)") | .num | tonumber) else null end),
        urgency: (.urgency | if . then .value else null end),
        impact: (.impact | if . then .value else null end),
        reach: (.reach | if . then .value else null end),
        size: (.size | if . then .value else null end),
        difficulty: (.difficulty | if . then .value else null end),
        confidence: (.confidence | if . then .value else null end),
        theme: (.theme | if . then .value else null end),
        score: (.score | if . then .value else null end),
        title: .content.title,
        createdAt: .content.createdAt,
        assignee: .content.assignees.nodes[0].login,
        body: .content.body,
        content: {
          type: .type,
          value: (
            if .type == "DRAFT_ISSUE" then
              {
                id: .content.id
              }
            elif .type == "ISSUE" then
              {
                id: .content.id,
                repository: .content.repository.nameWithOwner,
                number: .content.number,
                url: .content.url,
                issueType: (.content.issueType | if . then .name else null end),
                state: .content.state,
                stateReason: (.content.stateReason | if . then . else null end)

              }
            elif .type == "PULL_REQUEST" then
              {
                id: .content.id,
                repository: .content.repository.nameWithOwner,
                number: .content.number,
                url: .content.url,
                state: .content.state
              }
            else
              null
            end
          )
        }
      }
;

def extract_project_items:
  [.[].data.owner.projectV2.items.nodes[] | extract_project_item] | flatten
;

. as $data
  | ($data[0] | extract_organization_info) as $owner
  | ($data[0] | extract_project_info) as $project
  | ($data | extract_project_items) as $items
  | {
      owner: $owner,
      meta: $project,
      items: $items
    }
