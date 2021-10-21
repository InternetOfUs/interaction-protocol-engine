/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine.prolog;

import eu.internetofus.common.components.models.SocialNetworkRelationship;
import eu.internetofus.common.components.models.SocialNetworkRelationshipType;
import eu.internetofus.common.components.models.Task;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.WeNetUserProfile;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.task_manager.TaskPredicates;
import eu.internetofus.common.components.task_manager.TaskTransactionPredicates;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

/**
 * Test the condition to calculate the normalized socialness.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class WhoToAskDifferentSocialClosenessIT extends AbstractWhoToAskITC {

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doBeforeTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    final var profile = this.users.get(0);
    profile.relationships = new ArrayList<>();
    for (var i = 1; i < this.users.size() - 1; i++) {

      final var relationship = new SocialNetworkRelationship();
      relationship.appId = this.app.appId;
      relationship.type = SocialNetworkRelationshipType.values()[i % SocialNetworkRelationshipType.values().length];
      relationship.userId = this.users.get(i).id;
      relationship.weight = 1.0 - 0.1 * (i + 1);
      profile.relationships.add(relationship);

    }

    return WeNetProfileManager.createProxy(vertx).updateProfile(profile).map(updated -> {
      this.users.remove(0);
      this.users.add(0, updated);
      return null;
    });

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Task createTaskForProtocol() {

    final var task = super.createTaskForProtocol();
    task.attributes.put("socialCloseness", "different").put("maxUsers", 2);
    return task;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doAfterTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    return this.waitUntilUserTaskState(this.task.requesterId, vertx, testContext, userTaskState -> {

      if (userTaskState.attributes != null) {

        final var socialClosenessUsers = userTaskState.attributes.getJsonArray("socialClosenessUsers");
        for (var i = this.users.size() - 1; i > 0; i--) {

          final var userId = this.users.get(i).id;
          final var value = 100 - (9 - i) * 10;
          var found = false;
          for (var j = 0; j < socialClosenessUsers.size(); j++) {

            final var element = socialClosenessUsers.getJsonObject(j);
            if (userId.equals(element.getString("userId"))) {

              found = Math.round(element.getDouble("value") * 100) == value;
              break;
            }
          }
          if (!found) {

            return false;
          }
        }

        final var whoToAsk = userTaskState.attributes.getJsonArray("whoToAskUsers");
        var j = 0;
        for (var i = this.users.size() - 1; i > 0; i--, j++) {

          final var userId = this.users.get(i).id;
          final var value = 100 - (9 - i) * 10;
          final var element = whoToAsk.getJsonObject(j);
          if (!userId.equals(element.getString("userId")) && Math.round(element.getDouble("value") * 100) != value) {

            return false;
          }
        }

        final var unaskedUserIds = userTaskState.attributes.getJsonArray("unaskedUserIds");
        j = 0;
        for (var i = this.users.size() - 3; i > 0; i--, j++) {

          final var userId = this.users.get(i).id;
          if (!userId.equals(unaskedUserIds.getString(j))) {

            return false;
          }
        }

        return true;
      }

      return false;

    }).compose(
        ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, this.users.get(9), this.users.get(8)));

  }

  /**
   * Check that can ask to more users.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(6)
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  public void shouldAskMoreUsers(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(5, testContext);

    final var moreAnswerTransaction = new TaskTransaction();
    moreAnswerTransaction.taskId = this.task.id;
    moreAnswerTransaction.actioneerId = this.task.requesterId;
    moreAnswerTransaction.label = "moreAnswerTransaction";

    final var checkTask = this.createTaskPredicate()
        .and(TaskPredicates.lastTransactionIs(
            this.createTaskTransactionPredicate().and(TaskTransactionPredicates.similarTo(moreAnswerTransaction))
                .and(TaskTransactionPredicates.messagesSizeIs(1))));

    WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction)
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(2))))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, this.users.get(7),
            this.users.get(6)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(3))))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, this.users.get(5),
            this.users.get(4)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(4))))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, this.users.get(3),
            this.users.get(2)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(5))))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, this.users.get(1)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(6))))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, new WeNetUserProfile[0]))
        .onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));

  }

}
