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

import eu.internetofus.common.components.models.Task;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.WeNetUserProfile;
import eu.internetofus.common.components.task_manager.TaskPredicates;
import eu.internetofus.common.components.task_manager.TaskTransactionPredicates;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

/**
 * Test the who to ask when the domain interest is different.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class WhoToAskDifferentDomainInterestIT extends AbstractWhoToAskITC {

  /**
   * The users to ask.
   */
  protected List<WeNetUserProfile> expectedWhoToAskUsers = new ArrayList<>();

  /**
   * {@inheritDoc}
   */
  @Override
  protected Task createTaskForProtocol() {

    final var task = super.createTaskForProtocol();
    task.attributes.put("domain", "studying_career").put("domainInterest", "different").put("maxUsers", 2);
    return task;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doAfterTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    return this.waitUntilUserTaskState(this.task.requesterId, vertx, testContext, userTaskState -> {

      if (userTaskState.attributes != null) {

        final var domainInterestUsers = userTaskState.attributes.getJsonArray("domainInterestUsers");
        final var whoToAskUsers = userTaskState.attributes.getJsonArray("whoToAskUsers");
        final var unaskedUserIds = userTaskState.attributes.getJsonArray("unaskedUserIds");
        final var maxUsers = this.users.size();
        if (domainInterestUsers == null || maxUsers - 1 != domainInterestUsers.size() || whoToAskUsers == null
            || maxUsers - 1 != whoToAskUsers.size() || unaskedUserIds == null || maxUsers - 3 != unaskedUserIds.size()

        ) {

          return false;
        }

        var expectedUsers = new ArrayList<>(this.users.subList(1, maxUsers));
        for (var i = 0; i < maxUsers - 1; i++) {

          final var element = domainInterestUsers.getJsonObject(i);
          final var userId = element.getString("userId");
          final var value = element.getDouble("value");
          for (var j = 0; j < expectedUsers.size(); j++) {

            final var expectedUser = expectedUsers.get(j);
            if (expectedUser.id.equals(userId)) {

              final var expectedValue = Math
                  .abs(expectedUser.competences.get(0).level - this.users.get(0).competences.get(0).level) / 2.0;
              if (Math.abs(expectedValue - value) > 0.00001d) {

                return false;

              } else {

                expectedUsers.remove(j);
                break;
              }
            }

          }
        }
        if (!expectedUsers.isEmpty()) {

          return false;
        }

        this.expectedWhoToAskUsers.clear();
        expectedUsers = new ArrayList<>(this.users.subList(1, maxUsers));
        var max = 1.0d;
        for (var i = 0; i < maxUsers - 1; i++) {

          final var element = whoToAskUsers.getJsonObject(i);
          final var userId = element.getString("userId");
          final var value = element.getDouble("value");
          for (var j = 0; j < expectedUsers.size(); j++) {

            final var expectedUser = expectedUsers.get(j);
            if (expectedUser.id.equals(userId)) {

              final var expectedValue = Math
                  .abs(expectedUser.competences.get(0).level - this.users.get(0).competences.get(0).level) / 2.0;
              if (Math.abs(expectedValue - value) > 0.00001d || value > max) {

                return false;

              } else if (i >= 2 && !expectedUser.id.equals(unaskedUserIds.getString(i - 2))) {

                return false;

              } else {

                max = value;
                this.expectedWhoToAskUsers.add(expectedUsers.remove(j));
                break;
              }

            }
          }
        }

        if (!expectedUsers.isEmpty()) {

          return false;
        }

        return true;
      }

      return false;

    }).compose(ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true, this.expectedWhoToAskUsers.get(0),
        this.expectedWhoToAskUsers.get(1)));

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
        .compose(ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true,
            this.expectedWhoToAskUsers.get(2), this.expectedWhoToAskUsers.get(3)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(3))))
        .compose(ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true,
            this.expectedWhoToAskUsers.get(4), this.expectedWhoToAskUsers.get(5)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(4))))
        .compose(ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true,
            this.expectedWhoToAskUsers.get(6), this.expectedWhoToAskUsers.get(7)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(5))))
        .compose(
            ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true, this.expectedWhoToAskUsers.get(8)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(6))))
        .compose(ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true, new WeNetUserProfile[0]))
        .onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));

  }

}
